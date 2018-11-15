//! Extra system utilities.
//!
//! This modules contains an extension trait for the System trait which adds useful transformation
//! functions.

use ecs::prelude::{Read, System};
use shred::{RunningTime, SystemData};
use std::collections::HashSet;
use std::hash;

/// Extra functionality associated systems.
pub trait SystemExtra {
    /// Build a system that will do nothing unless a specific resource matches a predicate.
    ///
    /// # Examples
    ///
    /// ## Enabling a single system at a time
    ///
    /// ```rust
    /// use amethyst::{
    ///     ecs::{System, Write},
    ///     shred::DispatcherBuilder,
    ///     prelude::*,
    /// };
    ///
    /// #[derive(Hash, PartialEq, Eq)]
    /// enum State {
    ///     Other,
    ///     One,
    ///     Two,
    /// }
    ///
    /// impl Default for State {
    ///     fn default() -> Self {
    ///         State::Other
    ///     }
    /// }
    ///
    /// struct AddNumber(u32);
    ///
    /// impl<'s> System<'s> for AddNumber {
    ///     type SystemData = Write<'s, u32>;
    ///
    ///     fn run(&mut self, mut number: Self::SystemData) {
    ///         *number += self.0;
    ///     }
    /// }
    ///
    /// let mut world = World::new();
    ///
    /// let mut dispatcher = DispatcherBuilder::default()
    ///     .with(AddNumber(1), "set_number", &[])
    ///     .with(AddNumber(2).enabled_on(IsEq::new(State::One)), "set_number_1", &[])
    ///     .with(AddNumber(4).enabled_on(IsEq::new(State::Two)), "set_number_2", &[])
    ///     .build();
    ///
    /// dispatcher.setup(&mut world.res);
    ///
    /// // No systems are enabled.
    /// *world.write_resource() = 0u32;
    /// dispatcher.dispatch(&mut world.res);
    /// assert_eq!(1, *world.read_resource::<u32>());
    ///
    /// // Enable a single system.
    /// *world.write_resource() = 0u32;
    /// *world.write_resource() = State::One;
    /// dispatcher.dispatch(&mut world.res);
    /// assert_eq!(1 + 2, *world.read_resource::<u32>());
    ///
    /// *world.write_resource() = 0u32;
    /// *world.write_resource() = State::Two;
    /// dispatcher.dispatch(&mut world.res);
    /// assert_eq!(1 + 4, *world.read_resource::<u32>());
    /// ```
    ///
    /// ## Enabling multiple systems at a time
    ///
    /// ```rust
    /// use amethyst::{
    ///     ecs::{System, Write},
    ///     shred::DispatcherBuilder,
    ///     prelude::*,
    /// };
    /// use std::collections::HashSet;
    ///
    /// #[derive(Hash, PartialEq, Eq)]
    /// enum State {
    ///     Other,
    ///     One,
    ///     Two,
    /// }
    ///
    /// impl Default for State {
    ///     fn default() -> Self {
    ///         State::Other
    ///     }
    /// }
    ///
    /// struct AddNumber(u32);
    ///
    /// impl<'s> System<'s> for AddNumber {
    ///     type SystemData = Write<'s, u32>;
    ///
    ///     fn run(&mut self, mut number: Self::SystemData) {
    ///         *number += self.0;
    ///     }
    /// }
    ///
    /// let mut world = World::new();
    ///
    /// let mut dispatcher = DispatcherBuilder::default()
    ///     .with(AddNumber(1), "set_number", &[])
    ///     .with(AddNumber(2).enabled_on(OneOf::new(vec![State::One, State::Two])), "set_number_1", &[])
    ///     .with(AddNumber(4).enabled_on(OneOf::new(vec![State::One, State::Two])), "set_number_2", &[])
    ///     .build();
    ///
    /// dispatcher.setup(&mut world.res);
    ///
    /// // No systems are enabled.
    /// *world.write_resource() = 0u32;
    /// dispatcher.dispatch(&mut world.res);
    /// assert_eq!(1, *world.read_resource::<u32>());
    ///
    /// // Enable a single state.
    /// *world.write_resource() = 0u32;
    /// *world.write_resource() = State::One;
    /// dispatcher.dispatch(&mut world.res);
    /// assert_eq!(1 + 2 + 4, *world.read_resource::<u32>());
    ///
    /// // Enable two states.
    /// *world.write_resource() = 0u32;
    /// *world.write_resource() = State::Two;
    /// dispatcher.dispatch(&mut world.res);
    /// assert_eq!(1 + 2 + 4, *world.read_resource::<u32>());
    /// ```
    fn enabled_on<T: 'static>(self, value: T) -> EnabledOn<Self, T>
    where
        Self: Sized,
        T: Predicate,
        T::Value: Send + Sync + Default;
}

impl<'s, S> SystemExtra for S
where
    S: System<'s>,
{
    fn enabled_on<T: 'static>(self, value: T) -> EnabledOn<Self, T>
    where
        Self: Sized,
        T: Predicate,
        T::Value: Send + Sync + Default,
    {
        EnabledOn(self, value)
    }
}

/// A system that is enabled when `T` has a specific value.
pub struct EnabledOn<S, T>(S, T);

impl<'s, S, T: 'static> System<'s> for EnabledOn<S, T>
where
    S::SystemData: SystemData<'s>,
    S: System<'s>,
    T: Predicate,
    T::Value: Send + Sync + Default,
{
    type SystemData = (Read<'s, T::Value>, S::SystemData);

    fn run(&mut self, data: Self::SystemData) {
        if !self.1.test(&data.0) {
            return;
        }

        self.0.run(data.1);
    }

    fn running_time(&self) -> RunningTime {
        self.0.running_time()
    }
}

pub trait Predicate {
    type Value;

    /// Test if the predicate is true against the specified value.
    fn test(&self, value: &Self::Value) -> bool;
}

/// An [`Predicate`] implementation that tests that a single value is equal to another.
pub struct OneOf<T>(HashSet<T>);

impl<T> OneOf<T>
where
    T: hash::Hash + Eq,
{
    /// Build a new OneOf tester.
    pub fn new(iter: impl IntoIterator<Item = T>) -> Self {
        OneOf(iter.into_iter().collect::<HashSet<_>>())
    }
}

impl<T> Predicate for OneOf<T>
where
    T: hash::Hash + Eq,
{
    type Value = T;

    fn test(&self, value: &Self::Value) -> bool {
        self.0.contains(value)
    }
}

/// An [`Predicate`] implementation that tests that a single value is equal to another.
pub struct IsEq<T>(T);

impl<T> IsEq<T>
where
    T: Eq,
{
    /// Build a new IsEq tester.
    pub fn new(value: T) -> Self {
        IsEq(value)
    }
}

impl<T> Predicate for IsEq<T>
where
    T: Eq,
{
    type Value = T;

    fn test(&self, value: &Self::Value) -> bool {
        self.0 == *value
    }
}
