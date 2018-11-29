use amethyst::prelude::*;

/// State that a specific transition on `.update()`.
#[derive(Debug)]
pub struct ReturnState<S>(Trans<S>);

impl<S, E> StateCallback<S, E> for ReturnState<S>
where
    S: 'static + Copy,
    E: 'static + Send + Sync,
{
    fn update(&mut self, _: &mut World) -> Trans<S> {
        self.0
    }
}
