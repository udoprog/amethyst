use amethyst::prelude::*;

/// State that a specific transition on `.update()`.
#[derive(Debug)]
pub struct ReturnState<S>(pub Trans<S>);

impl<S, E> StateHandler<S, E> for ReturnState<S>
where
    S: 'static + Copy,
{
    fn update(&mut self, _: &mut World) -> Trans<S> {
        self.0
    }
}

impl<S, E> GlobalHandler<S, E> for ReturnState<S>
where
    S: 'static + Copy,
{
    fn update(&mut self, _: &mut World) -> Trans<S> {
        self.0
    }
}
