//! Extra system utilities.
//!
//! This modules contains an extension trait for the System trait which adds useful transformation
//! functions.

use ecs::prelude::{System, Read};
use shred::{SystemData, RunningTime};

/// Extra functionality associated systems.
pub trait SystemExtra {
    /// Build a system that will do nothing unless the `V` resource the specified value.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use amethyst::{
    ///     ecs::{System, Write},
    ///     shred::DispatcherBuilder,
    ///     prelude::*,
    /// };
    ///
    /// #[derive(PartialEq, Eq)]
    /// enum State {
    ///     Other,
    ///     Example,
    /// }
    ///
    /// impl Default for State {
    ///     fn default() -> Self {
    ///         State::Other
    ///     }
    /// }
    ///
    /// struct Example(u32);
    ///
    /// impl<'s> System<'s> for Example {
    ///     type SystemData = Write<'s, u32>;
    ///
    ///     fn run(&mut self, mut number: Self::SystemData) {
    ///         *number = self.0;
    ///     }
    /// }
    ///
    /// let mut world = World::new();
    ///
    /// let mut dispatcher = DispatcherBuilder::default()
    ///     .with(Example(42), "set_number", &[])
    ///     .with(Example(84).enabled_on(State::Example), "set_number_2", &[])
    ///     .build();
    ///
    /// dispatcher.setup(&mut world.res);
    ///
    /// // we only expect the u32 resource to be modified _if_ the system is enabled,
    /// // the system should only be enabled on State::Example.
    ///
    /// *world.write_resource() = 1u32;
    /// dispatcher.dispatch(&mut world.res);
    /// assert_eq!(42, *world.read_resource::<u32>());
    ///
    /// *world.write_resource::<State>() = State::Example;
    /// dispatcher.dispatch(&mut world.res);
    /// assert_eq!(84, *world.read_resource::<u32>());
    /// ```
    fn enabled_on<V: 'static>(self, value: V) -> EnabledOn<Self, V>
        where Self: Sized,
              V: Send + Sync + Default + Eq;
}

impl<'s, S> SystemExtra for S
    where S: System<'s>
{
    fn enabled_on<V: 'static>(self, value: V) -> EnabledOn<Self, V>
        where Self: Sized,
              V: Send + Sync + Default + Eq
    {
        EnabledOn(self, value)
    }
}

/// A system that is enabled when `U` has a specific value.
pub struct EnabledOn<S, V>(S, V);

impl<'s, S, V: 'static> System<'s> for EnabledOn<S, V>
    where
        S::SystemData: SystemData<'s>,
        S: System<'s>,
        V: Send + Sync + Default + Eq,
{
    type SystemData = (Read<'s, V>, S::SystemData);

    fn run(&mut self, data: Self::SystemData) {
        if self.1 != *data.0 {
            return;
        }

        self.0.run(data.1);
    }

    fn running_time(&self) -> RunningTime {
        self.0.running_time()
    }
}
