//! Dynamic utilities for game state management.

use crate::ecs::prelude::World;

use std::fmt::Result as FmtResult;
use std::fmt::{Display, Formatter};
use std::hash::Hash;
use hashbrown::HashMap;
use smallvec::SmallVec;

/// Error type for errors occurring in StateMachine
#[derive(Debug)]
pub enum StateError {
    /// Error raised when no states are present.
    NoStatesPresent,
}

impl Display for StateError {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> FmtResult {
        match *self {
            StateError::NoStatesPresent => write!(
                fmt,
                "Tried to start state machine without any states present"
            ),
        }
    }
}

/// Types of state transitions.
/// `S` is the state this state machine deals with.
pub enum Trans<S> {
    /// Continue as normal.
    None,
    /// Remove the active state and resume the next state on the stack or stop
    /// if there are none.
    Pop,
    /// Pause the active state and push a new state onto the stack.
    Push(S),
    /// Remove the current state on the stack and insert a different one.
    Switch(S),
    /// Stop and remove all states and shut down the engine.
    Quit,
}

/// Event queue to trigger state `Trans` from other places than a `State`'s methods.
/// # Example:
/// ```rust, ignore
/// world.write_resource::<EventChannel<TransEvent<MyGameData, StateEvent>>>().single_write(Box::new(|| Trans::Quit));
/// ```
///
/// Transitions will be executed sequentially by Amethyst's `CoreApplication` update loop.
pub type TransEvent<S> = Box<dyn Fn() -> Trans<S> + Send + Sync + 'static>;

/// How many callbacks which are inlined.
const INLINED_CALLBACKS: usize = 16;

type Callback = Box<dyn for<'world> Fn(&'world mut World)>;
type Callbacks = SmallVec<[Callback; INLINED_CALLBACKS]>;
type EventCallback<S, E> = Box<dyn for<'world> Fn(&'world mut World, &E) -> Trans<S>>;
type EventCallbacks<S, E> = SmallVec<[EventCallback<S, E>; INLINED_CALLBACKS]>;

/// A simple stack-based state machine (pushdown automaton).
#[derive(Derivative)]
#[derivative(Debug)]
pub struct StateMachine<S, E> {
    running: bool,
    #[derivative(Debug = "ignore")]
    state_stack: Vec<S>,
    #[derivative(Debug = "ignore")]
    on_start: HashMap<S, Callbacks>,
    #[derivative(Debug = "ignore")]
    on_stop: HashMap<S, Callbacks>,
    #[derivative(Debug = "ignore")]
    on_pause: HashMap<S, Callbacks>,
    #[derivative(Debug = "ignore")]
    on_resume: HashMap<S, Callbacks>,
    #[derivative(Debug = "ignore")]
    handle_event: HashMap<S, EventCallbacks<S, E>>,
    /// Callbacks fired on any state.
    #[derivative(Debug = "ignore")]
    any_handle_event: EventCallbacks<S, E>,
    #[derivative(Debug = "ignore")]
    fixed_update: HashMap<S, Callbacks>,
    #[derivative(Debug = "ignore")]
    shadow_fixed_update: HashMap<S, Callbacks>,
    #[derivative(Debug = "ignore")]
    update: HashMap<S, Callbacks>,
    #[derivative(Debug = "ignore")]
    shadow_update: HashMap<S, Callbacks>,
}

/// Helper macros to build callback registration functions.
macro_rules! callbacks {
    ($(($fn:ident, $name:ident),)*) => {
        $(
        #[doc = "Register a callback for the specified event and state."]
        pub fn $fn<C: 'static>(&mut self, state: S, callback: C)
            where C: for<'world> Fn(&'world mut World)
        {
            self.$name.entry(state).or_default().push(Box::new(callback));
        }
        )*
    }
}

/// Helper macros to build event callback registration functions.
macro_rules! event_callbacks {
    ($(($fn:ident, $name:ident),)*) => {
        $(
        #[doc = "Register a callback for the specified event and state."]
        pub fn $fn<C: 'static>(&mut self, state: S, callback: C)
            where C: for<'world> Fn(&'world mut World, &E) -> Trans<S>
        {
            self.$name.entry(state).or_default().push(Box::new(callback));
        }
        )*
    }
}

/// Helper macros to build event callback registration functions.
macro_rules! any_event_callbacks {
    ($(($fn:ident, $name:ident),)*) => {
        $(
        #[doc = "Register an event handler callback that applies for any states of a specific event."]
        pub fn $fn<C: 'static>(&mut self, callback: C)
            where C: for<'world> Fn(&'world mut World, &E) -> Trans<S>
        {
            self.$name.push(Box::new(callback));
        }
        )*
    }
}

impl<S, E> StateMachine<S, E> where S: Copy + PartialEq + Eq + Hash {
    /// Creates a new state machine with the given initial state.
    pub fn new(initial_state: S) -> StateMachine<S, E> {
        StateMachine {
            running: false,
            state_stack: vec![initial_state],
            on_start: Default::default(),
            on_stop: Default::default(),
            on_pause: Default::default(),
            on_resume: Default::default(),
            handle_event: Default::default(),
            any_handle_event: Default::default(),
            fixed_update: Default::default(),
            shadow_fixed_update: Default::default(),
            update: Default::default(),
            shadow_update: Default::default(),
        }
    }

    callbacks![
        (register_on_start, on_start),
        (register_on_stop, on_stop),
        (register_on_pause, on_pause),
        (register_on_resume, on_resume),
        (register_fixed_update, fixed_update),
        (register_shadow_fixed_update, shadow_fixed_update),
        (register_update, update),
        (register_shadow_update, shadow_update),
    ];

    event_callbacks![
        (register_handle_event, handle_event),
    ];

    any_event_callbacks![
        (register_any_handle_event, any_handle_event),
    ];

    /// Checks whether the state machine is running.
    pub fn is_running(&self) -> bool {
        self.running
    }

    /// Initializes the state machine.
    pub fn start(&mut self, world: &mut World) -> Result<(), StateError> {
        if !self.running {
            let state = self
                .state_stack
                .last()
                .ok_or(StateError::NoStatesPresent)?;

            if let Some(on_start) = self.on_start.get(state) {
                Self::run(on_start, |c| c(world));
            }

            self.running = true;
        }
        Ok(())
    }

    /// Passes a single event to the active state to handle.
    pub fn handle_event(&mut self, world: &mut World, event: E) {
        if !self.running {
            return;
        }

        // Transitions which have been requested by callbacks.
        let mut transitions = SmallVec::<[Trans<S>; INLINED_CALLBACKS * 2]>::new();

        {
            let handle_event = self.state_stack.last().and_then(|s| self.handle_event.get(s));

            if let Some(handle_event) = handle_event {
                Self::run(handle_event, |c| {
                    transitions.push(c(world, &event));
                });
            }
        }

        Self::run(&self.any_handle_event, |c| {
            transitions.push(c(world, &event));
        });

        for trans in transitions {
            self.transition(trans, world);
        }
    }

    /// Updates the currently active state at a steady, fixed interval.
    pub fn fixed_update(&mut self, world: &mut World) {
        if self.running {
            let fixed_update = self.state_stack.last().and_then(|s| self.fixed_update.get(s));

            if let Some(fixed_update) = fixed_update {
                Self::run(fixed_update, |c| c(world));
            }

            for callbacks in self.shadow_fixed_update.values() {
                Self::run(callbacks, |c| c(world));
            }

            // NB: transition not possible.
            // self.transition(trans, world);
        }
    }

    /// Updates the currently active state immediately.
    pub fn update(&mut self, world: &mut World) {
        if self.running {
            let update = self.state_stack.last().and_then(|s| self.update.get(s));

            if let Some(update) = update {
                Self::run(update, |c| c(world));
            }

            for callbacks in self.shadow_update.values() {
                Self::run(callbacks, |c| c(world));
            }

            // NB: transition not possible.
            // self.transition(trans, world);
        }
    }

    /// Performs a state transition.
    pub fn transition(&mut self, request: Trans<S>, world: &mut World) {
        if self.running {
            match request {
                Trans::None => (),
                Trans::Pop => self.pop(world),
                Trans::Push(state) => self.push(state, world),
                Trans::Switch(state) => self.switch(state, world),
                Trans::Quit => self.stop(world),
            }
        }
    }

    /// Removes the current state on the stack and inserts a different one.
    fn switch(&mut self, state: S, world: &mut World) {
        if self.running {
            if let Some(on_stop) = self.state_stack.pop().and_then(|s| self.on_stop.get(&s)) {
                Self::run(&on_stop, |c| c(world));
            }

            self.state_stack.push(state);

            if let Some(on_start) = self.on_start.get(&state) {
                Self::run(on_start, |c| c(world));
            }
        }
    }

    /// Pauses the active state and pushes a new state onto the state stack.
    fn push(&mut self, state: S, world: &mut World) {
        if self.running {
            if let Some(on_pause) = self.state_stack.last().and_then(|s| self.on_pause.get(s)) {
                Self::run(on_pause, |c| c(world));
            }

            self.state_stack.push(state);

            if let Some(on_start) = self.on_start.get(&state) {
                Self::run(on_start, |c| c(world));
            }
        }
    }

    /// Stops and removes the active state and un-pauses the next state on the
    /// stack (if any).
    fn pop(&mut self, world: &mut World) {
        if self.running {
            if let Some(on_stop) = self.state_stack.pop().and_then(|s| self.on_stop.get(&s)) {
                Self::run(on_stop, |c| c(world));
            }

            if let Some(state) = self.state_stack.last() {
                if let Some(on_resume) = self.on_resume.get(state) {
                    Self::run(on_resume, |c| c(world));
                }
            } else {
                self.running = false;
            }
        }
    }

    /// Shuts the state machine down.
    pub(crate) fn stop(&mut self, world: &mut World) {
        if self.running {
            while let Some(on_stop) = self.state_stack.pop().and_then(|s| self.on_stop.get(&s)) {
                Self::run(on_stop, |c| c(world));
            }

            self.running = false;
        }
    }

    /// Helper to run a collection of callbacks.
    #[inline]
    fn run<C>(callbacks: &[C], mut apply: impl FnMut(&C)) {
        for c in callbacks {
            apply(c);
        }
    }
}
