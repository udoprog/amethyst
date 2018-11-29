use amethyst::prelude::*;

/// `Push`es each `State` onto the Amethyst state stack in reverse order (LIFO).
///
/// This implementation does not override the `Trans`ition returned by the `State` that is pushed
/// to. Furthermore, it always transitions to the next state in the stack, which means there is no
/// "opt-out" of going through the stack.
#[derive(Derivative, new)]
#[derivative(Debug)]
pub struct SequencerState<S> {
    /// States to switch through, in reverse order.
    #[derivative(Debug = "ignore")]
    states: Vec<S>,
}

impl<S, E> GlobalCallback<S, E> for SequencerState<S>
where
    S: 'static + Send + Sync + State<E>,
    E: 'static + Send + Sync,
{
    fn update(&mut self, _: &mut World) -> Trans<S> {
        if let Some(state) = self.states.pop() {
            Trans::Push(state)
        } else {
            Trans::Pop
        }
    }
}
