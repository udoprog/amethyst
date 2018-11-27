//! The core engine framework.

use std::{error::Error as StdError, marker::PhantomData, path::Path, sync::Arc, time::Duration, hash::Hash};

use crate::shred::Resource;
use log::Level;
use rayon::ThreadPoolBuilder;

#[cfg(feature = "profiler")]
use thread_profiler::{register_thread_with_profiler, write_profile};
use winit::Event;

use crate::{
    assets::{Loader, Source},
    callback_queue::CallbackQueue,
    core::{
        frame_limiter::{FrameLimiter, FrameRateLimitConfig, FrameRateLimitStrategy},
        shrev::{EventChannel, ReaderId},
        timing::{Stopwatch, Time},
        EventReader, Named,
    },
    ecs::{
        common::Errors,
        prelude::{Component, Read, World, Write},
    },
    error::{Error, Result},
    dynamic::state::{StateMachine, TransEvent, Trans},
    state_event::{StateEvent, StateEventReader},
    ui::UiEvent,
};

/// `CoreApplication` is the application implementation for the game engine. This is fully generic
/// over the state type and event type.
///
/// When starting out with Amethyst, use the type alias `Application`, which have sensible defaults
/// for the `Event` and `EventReader` generic types.
///
/// ### Type parameters:
///
/// - `S`: `State`
/// - `E`: `Event` type that should be sent to the states
/// - `R`: `EventReader` implementation for the given event type `E`
#[derive(Derivative)]
#[derivative(Debug)]
pub struct CoreApplication<S, E = StateEvent, R = StateEventReader>
where
    S: 'static,
    E: 'static,
{
    /// The world
    #[derivative(Debug = "ignore")]
    world: World,
    #[derivative(Debug = "ignore")]
    reader: R,
    #[derivative(Debug = "ignore")]
    events: Vec<E>,
    event_reader_id: ReaderId<Event>,
    #[derivative(Debug = "ignore")]
    trans_reader_id: ReaderId<TransEvent<S>>,
    states: StateMachine<S, E>,
    ignore_window_close: bool,
}

/// An Application is the root object of the game engine. It binds the OS
/// event loop, state machines, timers and other core components in a central place.
///
/// Since Application functions as the root of the game, Amethyst does not need
/// to use any global variables. Within this object is everything that your
/// game needs to run.
///
/// # Logging
///
/// Amethyst performs logging internally using the [log] crate. By default, `Application` will
/// initialize a global logger that simply sends logs to the console. You can take advantage of
/// this and use the logging macros in `log` once you've created your `Application` instance:
///
/// ```
/// extern crate amethyst;
/// #[macro_use]
/// extern crate log;
///
/// use amethyst::prelude::*;
/// use amethyst::core::transform::{Parent, Transform};
/// use amethyst::ecs::prelude::System;
///
/// struct NullState;
/// impl EmptyState for NullState {}
///
/// fn main() -> amethyst::Result<()> {
///     amethyst::start_logger(Default::default());
///
///     // Build the application instance to initialize the default logger.
///     let mut game = Application::build("assets/", NullState)?
///         .build(())?;
///
///     // Now logging can be performed as normal.
///     info!("Using the default logger provided by amethyst");
///     warn!("Uh-oh, something went wrong!");
///
///     Ok(())
/// }
/// ```
///
/// You can also setup your own logging system. Simply intialize any global logger that supports
/// [log], and it will be used instead of the default logger:
///
/// ```
/// extern crate amethyst;
/// #[macro_use]
/// extern crate log;
/// extern crate env_logger;
///
/// use amethyst::prelude::*;
/// use amethyst::core::transform::{Parent, Transform};
/// use amethyst::ecs::prelude::System;
///
/// struct NullState;
/// impl EmptyState for NullState {}
///
/// fn main() -> amethyst::Result<()> {
///     // Initialize your custom logger (using env_logger in this case) before creating the
///     // `Application` instance.
///     env_logger::init();
///
///     // The default logger will be automatically disabled and any logging amethyst does
///     // will go through your custom logger.
///     let mut game = Application::build("assets/", NullState)?
///         .build(())?;
///
///     Ok(())
/// }
/// ```
///
/// [log]: https://crates.io/crates/log
pub type Application<S> = CoreApplication<S, StateEvent, StateEventReader>;

impl<S, E, R> CoreApplication<S, E, R>
where
    S: 'static + Copy + PartialEq + Eq + Hash,
    E: Clone + Send + Sync + 'static,
{
    /// Creates a new Application with the given initial game state.
    /// This will create and allocate all the needed resources for
    /// the event loop of the game engine. It is a shortcut for convenience
    /// if you need more control over how the engine is configured you should
    /// be using [build](struct.Application.html#method.build) instead.
    ///
    /// # Parameters
    ///
    /// - `path`: The default path for asset loading.
    ///
    /// - `initial_state`: The initial State handler of your game See
    ///   [State](trait.State.html) for more information on what this is.
    ///
    /// # Returns
    ///
    /// Returns a `Result` type wrapping the `Application` type. See
    /// [errors](struct.Application.html#errors) for a full list of
    /// possible errors that can happen in the creation of a Application object.
    ///
    /// # Type Parameters
    ///
    /// - `P`: The path type for your standard asset path.
    ///
    /// - `S`: A type that implements the `State` trait. e.g. Your initial
    ///        game logic.
    ///
    /// # Lifetimes
    ///
    /// - `a`: The lifetime of the `State` objects.
    /// - `b`: This lifetime is inherited from `specs` and `shred`, it is
    ///        the minimum lifetime of the systems used by `Application`
    ///
    /// # Errors
    ///
    /// Application will return an error if the internal thread pool fails
    /// to initialize correctly because of systems resource limitations
    pub fn new<P>(path: P, initial_state: S) -> Result<Self>
    where
        P: AsRef<Path>,
        for<'b> R: EventReader<'b, Event = E>,
        R: Default,
    {
        ApplicationBuilder::new(path, initial_state)?.build()
    }

    /// Creates a new ApplicationBuilder with the given initial game state.
    ///
    /// This is identical in function to
    /// [ApplicationBuilder::new](struct.ApplicationBuilder.html#method.new).
    pub fn build<P>(path: P, initial_state: S) -> Result<ApplicationBuilder<S, E, R>>
    where
        P: AsRef<Path>,
        for<'b> R: EventReader<'b, Event = E>,
    {
        ApplicationBuilder::new(path, initial_state)
    }

    /// Run the gameloop until the game state indicates that the game is no
    /// longer running. This is done via the `State` returning `Trans::Quit` or
    /// `Trans::Pop` on the last state in from the stack. See full
    /// documentation on this in [State](trait.State.html) documentation.
    pub fn run(&mut self)
    where
        for<'b> R: EventReader<'b, Event = E>,
    {
        self.initialize();
        self.world.write_resource::<Stopwatch>().start();
        while self.states.is_running() {
            self.advance_frame();

            self.world.write_resource::<FrameLimiter>().wait();
            {
                let elapsed = self.world.read_resource::<Stopwatch>().elapsed();
                let mut time = self.world.write_resource::<Time>();
                time.increment_frame_number();
                time.set_delta_time(elapsed);
            }
            let mut stopwatch = self.world.write_resource::<Stopwatch>();
            stopwatch.stop();
            stopwatch.restart();
        }

        self.shutdown();
    }

    /// Sets up the application.
    fn initialize(&mut self) {
        #[cfg(feature = "profiler")]
        profile_scope!("initialize");
        self.states.start(&mut self.world)
            .expect("Tried to start state machine without any states present");
    }

    // React to window close events
    fn should_close(&mut self) -> bool {
        if self.ignore_window_close {
            false
        } else {
            use crate::renderer::WindowEvent;
            let world = &mut self.world;
            let reader_id = &mut self.event_reader_id;
            world.exec(|ev: Read<'_, EventChannel<Event>>| {
                ev.read(reader_id).any(|e| {
                    if cfg!(target_os = "ios") {
                        if let Event::WindowEvent {
                            event: WindowEvent::Destroyed,
                            ..
                        } = e
                        {
                            true
                        } else {
                            false
                        }
                    } else {
                        if let Event::WindowEvent {
                            event: WindowEvent::CloseRequested,
                            ..
                        } = e
                        {
                            true
                        } else {
                            false
                        }
                    }
                })
            })
        }
    }

    /// Advances the game world by one tick.
    fn advance_frame(&mut self)
    where
        for<'b> R: EventReader<'b, Event = E>,
    {
        trace!("Advancing frame (`Application::advance_frame`)");
        if self.should_close() {
            let world = &mut self.world;
            let states = &mut self.states;
            states.stop(world);
        }

        // Read the Trans queue and apply changes.
        {
            let mut world = &mut self.world;
            let states = &mut self.states;
            let reader = &mut self.trans_reader_id;

            let trans = world
                .read_resource::<EventChannel<TransEvent<S>>>()
                .read(reader)
                .map(|e| e())
                .collect::<Vec<_>>();
            for tr in trans {
                states.transition(tr, &mut world);
            }
        }

        {
            #[cfg(feature = "profiler")]
            profile_scope!("run_callback_queue");
            let mut world = &mut self.world;
            let receiver = world.read_resource::<CallbackQueue>().receiver.clone();
            while let Ok(func) = receiver.try_recv() {
                func(&mut world);
            }
        }

        {
            #[cfg(feature = "profiler")]
            profile_scope!("handle_event");

            {
                let events = &mut self.events;
                self.reader.read(self.world.system_data(), events);
            }

            {
                let world = &mut self.world;
                let states = &mut self.states;
                for e in self.events.drain(..) {
                    states.handle_event(world, e);
                }
            }
        }
        {
            let do_fixed = {
                let time = self.world.write_resource::<Time>();
                time.last_fixed_update().elapsed() >= time.fixed_time()
            };
            #[cfg(feature = "profiler")]
            profile_scope!("fixed_update");
            if do_fixed {
                self.states.fixed_update(&mut self.world);
                self.world.write_resource::<Time>().finish_fixed_update();
            }

            #[cfg(feature = "profiler")]
            profile_scope!("update");
            self.states.update(&mut self.world);
        }

        #[cfg(feature = "profiler")]
        profile_scope!("maintain");
        self.world.maintain();

        // TODO: replace this with a more customizable method.
        // TODO: effectively, the user should have more control over error handling here
        // TODO: because right now the app will just exit in case of an error.
        self.world.write_resource::<Errors>().print_and_exit();
    }

    /// Cleans up after the quit signal is received.
    fn shutdown(&mut self) {
        info!("Engine is shutting down");

        // Placeholder.
    }
}

#[cfg(feature = "profiler")]
impl<S, E, R> Drop for CoreApplication<S, E, R> {
    fn drop(&mut self) {
        // TODO: Specify filename in config.
        use crate::utils::application_root_dir;
        let path = format!("{}/thread_profile.json", application_root_dir());
        write_profile(path.as_str());
    }
}

/// Helper macros to build callback registration functions.
macro_rules! state_machine_callbacks {
    ($($fn:ident,)*) => {
        $(
        #[doc = "Register a callback for the specified event and state."]
        pub fn $fn<C: 'static>(&mut self, state: S, callback: C)
            where C: for<'world> Fn(&'world mut World)
        {
            self.states.$fn(state, callback)
        }
        )*
    }
}

/// Helper macros to build event callback registration functions.
macro_rules! state_machine_event_callbacks {
    ($($fn:ident,)*) => {
        $(
        #[doc = "Register a callback for the specified event and state."]
        pub fn $fn<C: 'static>(&mut self, state: S, callback: C)
            where C: for<'world> Fn(&'world mut World, &E) -> Trans<S>
        {
            self.states.$fn(state, callback)
        }
        )*
    }
}

/// Helper macros to build event callback registration functions.
macro_rules! state_machine_any_event_callbacks {
    ($($fn:ident,)*) => {
        $(
        #[doc = "Register an event handler callback that applies for any states of a specific event."]
        pub fn $fn<C: 'static>(&mut self, callback: C)
            where C: for<'world> Fn(&'world mut World, &E) -> Trans<S>
        {
            self.states.$fn(callback);
        }
        )*
    }
}

/// `ApplicationBuilder` is an interface that allows for creation of an
/// [`Application`](struct.Application.html)
/// using a custom set of configuration. This is the normal way an
/// [`Application`](struct.Application.html)
/// object is created.
pub struct ApplicationBuilder<S, E, R> {
    /// Used by bundles to access the world directly
    pub world: World,
    ignore_window_close: bool,
    phantom: PhantomData<R>,
    states: StateMachine<S, E>,
}

impl<S> ApplicationBuilder<S, StateEvent, StateEventReader>
    where S: Copy + PartialEq + Eq + Hash
{
    /// Register default event handlers.
    pub fn register_default_handlers(&mut self) {
        // Handle close request on initial state.
        self.states.register_any_handle_event(|_, e| {
            use amethyst_input::is_close_requested;

            if let StateEvent::Window(ref event) = *e {
                if is_close_requested(event) {
                    Trans::Quit
                } else {
                    Trans::None
                }
            } else {
                Trans::None
            }
        });
    }
}

impl<S, E, R> ApplicationBuilder<S, E, R>
    where S: 'static + Copy + PartialEq + Eq + Hash
{
    /// Creates a new [ApplicationBuilder](struct.ApplicationBuilder.html) instance
    /// that wraps the initial_state. This is the more verbose way of initializing
    /// your application if you require specific configuration details to be changed
    /// away from the default.
    ///
    /// # Parameters
    /// - `initial_state`: The initial State handler of your game.
    ///
    /// # Returns
    ///
    /// Returns a `Result` type wrapping the `Application` type. See
    /// [errors](struct.Application.html#errors) for a full list of
    /// possible errors that can happen in the creation of a Application object.
    ///
    /// # Type parameters
    ///
    /// - `S`: A type that implements the `State` trait. e.g. Your initial
    ///        game logic.
    ///
    /// # Lifetimes
    ///
    /// - `a`: The lifetime of the `State` objects.
    /// - `b`: This lifetime is inherited from `specs` and `shred`, it is
    ///        the minimum lifetime of the systems used by `Application`
    ///
    /// # Errors
    ///
    /// Application will return an error if the internal threadpool fails
    /// to initialize correctly because of systems resource limitations
    pub fn new<P: AsRef<Path>>(path: P, initial_state: S) -> Result<Self> {
        use rustc_version_runtime;

        if !log_enabled!(Level::Error) {
            eprintln!(
                "WARNING: No logger detected! Did you forget to call `amethyst::start_logger()`?"
            );
        }

        info!("Initializing Amethyst...");
        info!("Version: {}", env!("CARGO_PKG_VERSION"));
        info!("Platform: {}", env!("VERGEN_TARGET_TRIPLE"));
        info!("Amethyst git commit: {}", env!("VERGEN_SHA"));
        let rustc_meta = rustc_version_runtime::version_meta();
        info!(
            "Rustc version: {} {:?}",
            rustc_meta.semver, rustc_meta.channel
        );
        if let Some(hash) = rustc_meta.commit_hash {
            info!("Rustc git commit: {}", hash);
        }

        let mut world = World::new();

        let thread_pool_builder = ThreadPoolBuilder::new();
        #[cfg(feature = "profiler")]
        let thread_pool_builder = thread_pool_builder.start_handler(|_index| {
            register_thread_with_profiler();
        });
        let pool = thread_pool_builder
            .build()
            .map(Arc::new)
            .map_err(|err| Error::Core(err.description().to_string().into()))?;
        world.add_resource(Loader::new(path.as_ref().to_owned(), pool.clone()));
        world.add_resource(pool);
        world.add_resource(EventChannel::<Event>::with_capacity(2000));
        world.add_resource(EventChannel::<UiEvent>::with_capacity(40));
        world.add_resource(EventChannel::<TransEvent<S>>::with_capacity(2));
        world.add_resource(Errors::default());
        world.add_resource(FrameLimiter::default());
        world.add_resource(Stopwatch::default());
        world.add_resource(Time::default());
        world.add_resource(CallbackQueue::default());

        world.register::<Named>();

        Ok(ApplicationBuilder {
            world,
            ignore_window_close: false,
            states: StateMachine::<S, E>::new(initial_state),
            phantom: PhantomData,
        })
    }

    state_machine_callbacks![
        register_on_start,
        register_on_stop,
        register_on_pause,
        register_on_resume,
        register_fixed_update,
        register_shadow_fixed_update,
        register_update,
        register_shadow_update,
    ];

    state_machine_event_callbacks![
        register_handle_event,
    ];

    state_machine_any_event_callbacks![
        register_any_handle_event,
    ];

    /// Registers a component into the entity-component-system. This method
    /// takes no options other than the component type which is defined
    /// using a 'turbofish'. See the example for what this looks like.
    ///
    /// You must register a component type before it can be used. If
    /// code accesses a component that has not previously been registered
    /// it will `panic`.
    ///
    /// # Type Parameters
    ///
    /// - `C`: The Component type that you are registering. This must
    ///        implement the `Component` trait to be registered.
    ///
    /// # Returns
    ///
    /// This function returns ApplicationBuilder after it has modified it
    pub fn register<C>(mut self) -> Self
    where
        C: Component,
        C::Storage: Default,
    {
        self.world.register::<C>();
        self
    }

    /// Adds the supplied ECS resource which can be accessed from game systems.
    ///
    /// Resources are common data that is shared with one or more game system.
    ///
    /// If a resource is added with the identical type as an existing resource,
    /// the new resource will replace the old one and the old resource will
    /// be dropped.
    ///
    /// # Parameters
    /// - `resource`: The initialized resource you wish to register
    ///
    /// # Type Parameters
    ///
    /// - `R`: `resource` must implement the `Resource` trait. This trait will
    ///      be automatically implemented if `Any` + `Send` + `Sync` traits
    ///      exist for type `R`.
    ///
    /// # Returns
    ///
    /// This function returns ApplicationBuilder after it has modified it.
    pub fn with_resource<T>(mut self, resource: T) -> Self
    where
        T: Resource,
    {
        self.world.add_resource(resource);
        self
    }

    /// Register an asset store with the loader logic of the Application.
    ///
    /// If the asset store exists, that shares a name with the new store the net
    /// effect will be a replacement of the older store with the new one.
    /// No warning or panic will result from this action.
    ///
    /// # Parameters
    ///
    /// - `name`: A unique name or key to identify the asset storage location. `name`
    ///           is used later to specify where the asset should be loaded from.
    /// - `store`: The asset store being registered.
    ///
    /// # Type Parameters
    ///
    /// - `I`: A `String`, or a type that can be converted into a`String`.
    /// - `S`: A `Store` asset loader. Typically this is a [`Directory`](../amethyst_assets/struct.Directory.html).
    ///
    /// # Returns
    ///
    /// This function returns ApplicationBuilder after it has modified it.
    pub fn with_source<I, O>(self, name: I, store: O) -> Self
    where
        I: Into<String>,
        O: Source,
    {
        {
            let mut loader = self.world.write_resource::<Loader>();
            loader.add_source(name, store);
        }
        self
    }

    /// Sets the maximum frames per second of this game.
    ///
    /// # Parameters
    ///
    /// `strategy`: the frame limit strategy to use
    /// `max_fps`: the maximum frames per second this game will run at.
    ///
    /// # Returns
    ///
    /// This function returns the ApplicationBuilder after modifying it.
    pub fn with_frame_limit(mut self, strategy: FrameRateLimitStrategy, max_fps: u32) -> Self {
        self.world
            .add_resource(FrameLimiter::new(strategy, max_fps));
        self
    }

    /// Sets the maximum frames per second of this game, based on the given config.
    ///
    /// # Parameters
    ///
    /// `config`: the frame limiter config
    ///
    /// # Returns
    ///
    /// This function returns the ApplicationBuilder after modifying it.
    pub fn with_frame_limit_config(mut self, config: FrameRateLimitConfig) -> Self {
        self.world.add_resource(FrameLimiter::from_config(config));
        self
    }

    /// Sets the duration between fixed updates, defaults to one sixtieth of a second.
    ///
    /// # Parameters
    ///
    /// `duration`: The duration between fixed updates.
    ///
    /// # Returns
    ///
    /// This function returns the ApplicationBuilder after modifying it.
    pub fn with_fixed_step_length(self, duration: Duration) -> Self {
        self.world.write_resource::<Time>().set_fixed_time(duration);
        self
    }

    /// Tells the resulting application window to ignore close events if ignore is true.
    /// This will make your game window unresponsive to operating system close commands.
    /// Use with caution.
    ///
    /// # Parameters
    ///
    /// `ignore`: Whether or not the window should ignore these events.  False by default.
    ///
    /// # Returns
    ///
    /// This function returns the ApplicationBuilder after modifying it.
    pub fn ignore_window_close(mut self, ignore: bool) -> Self {
        self.ignore_window_close = ignore;
        self
    }

    /// Build an `Application` object using the `ApplicationBuilder` as configured.
    ///
    /// # Returns
    ///
    /// This function returns an Application object wrapped in the Result type.
    ///
    /// # Errors
    ///
    /// This function currently will not produce an error, returning a result
    /// type was strictly for future possibilities.
    ///
    /// # Notes
    ///
    /// If the "profiler" feature is used, this function will register the thread
    /// that executed this function as the "Main" thread.
    pub fn build(mut self) -> Result<CoreApplication<S, E, R>>
    where
        S: Copy + PartialEq + Eq + Hash,
        E: Clone + Send + Sync + 'static,
        R: Default,
        for<'b> R: EventReader<'b, Event = E>,
    {
        trace!("Entering `ApplicationBuilder::build`");

        #[cfg(feature = "profiler")]
        register_thread_with_profiler();
        #[cfg(feature = "profiler")]
        profile_scope!("new");

        let mut reader = R::default();
        reader.setup(&mut self.world.res);
        let event_reader_id = self
            .world
            .exec(|mut ev: Write<'_, EventChannel<Event>>| ev.register_reader());

        let trans_reader_id = self
            .world
            .exec(|mut ev: Write<'_, EventChannel<TransEvent<S>>>| ev.register_reader());

        Ok(CoreApplication {
            world: self.world,
            states: self.states,
            reader,
            events: Vec::new(),
            ignore_window_close: self.ignore_window_close,
            event_reader_id,
            trans_reader_id,
        })
    }
}
