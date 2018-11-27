//! An example showcasing states.

use amethyst;
use amethyst::prelude::*;
use std::fmt;

#[derive(State, Debug, Clone, PartialEq, Eq, Hash, Default)]
struct Complex {
    name: String,
}

#[derive(State, Debug, Clone, PartialEq, Eq)]
enum State {
    A,
    Named(&'static str),
    Complex(Complex),
    StopIt,
}

/// An example handler that immediately switched the state to the next state.
#[derive(Debug)]
struct Example(State);

impl<E> StateHandler<State, E> for Example {
    fn on_start(&mut self, world: &mut World) {
        println!(
            "Started: {:?} in state {:?}",
            self,
            *world.read_resource::<State>()
        );
    }

    fn on_stop(&mut self, _: &mut World) {
        println!("Stop: {:?}", self);
    }

    fn update(&mut self, _: &mut World) -> Trans<State> {
        if self.0 == State::StopIt {
            return Trans::Quit;
        }

        println!("Switch: {:?}", self);
        Trans::Switch(self.0.clone())
    }
}

/// Monitors for state changes and prints a message.
struct ChangedMonitor;

impl<S, E> GlobalHandler<S, E> for ChangedMonitor
where
    S: 'static + fmt::Debug + Send + Sync,
{
    fn changed(&mut self, _: &mut World, state: &S) {
        println!("ChangedMonitor: Changed State: {:?}", state);
    }
}

/// Prints the given message on a specific state.
struct StatePrinter<S>(S, &'static str);

impl<S, E> GlobalHandler<S, E> for StatePrinter<S>
where
    S: 'static + Send + Sync + PartialEq + Eq,
{
    fn update(&mut self, world: &mut World) -> Trans<S> {
        if self.0 == *world.read_resource::<S>() {
            println!("StatePrinter: {}", self.1);
        }

        Trans::None
    }
}

fn main() -> amethyst::Result<()> {
    amethyst::start_logger(Default::default());

    let complex = Complex {
        name: String::from("Stephanie"),
    };

    let mut game = Application::build("./")?
        .with_state(State::A, Example(State::Named("foo")))?
        .with_state(State::Named("foo"), Example(State::Named("bar")))?
        .with_state(
            State::Named("bar"),
            Example(State::Complex(complex.clone())),
        )?
        .with_state(State::Complex(complex.clone()), Example(State::StopIt))?
        .with_global(StatePrinter(
            State::Named("bar"),
            "only running in the named `bar` state",
        ))
        .with_global(ChangedMonitor)
        .build(GameDataBuilder::default())?;

    game.run();
    Ok(())
}
