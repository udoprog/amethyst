//! Example showing how to load a Locale file as an Asset using the Loader.

extern crate amethyst;

use amethyst::{
    assets::{AssetStorage, Handle, Loader, Processor, ProgressCounter},
    ecs::{Read, ReadExpect},
    locale::*,
    prelude::*,
    utils::application_root_dir,
    Error,
};

struct Example {
    progress_counter: Option<ProgressCounter>,
    handle_en: Option<Handle<Locale>>,
    handle_fr: Option<Handle<Locale>>,
}

impl Example {
    pub fn new() -> Self {
        Example {
            progress_counter: None,
            handle_en: None,
            handle_fr: None,
        }
    }
}

impl<S, E> StateCallback<S, E> for Example {
    fn on_start(&mut self, world: &mut World) {
        world.add_resource(AssetStorage::<Locale>::new());
        let mut progress_counter = ProgressCounter::default();
        self.handle_en = Some(world.exec(
            |(loader, storage): (ReadExpect<Loader>, Read<AssetStorage<Locale>>)| {
                loader.load(
                    "locale/locale_en.ftl",
                    LocaleFormat,
                    (),
                    &mut progress_counter,
                    &storage,
                )
            },
        ));
        self.handle_fr = Some(world.exec(
            |(loader, storage): (ReadExpect<Loader>, Read<AssetStorage<Locale>>)| {
                loader.load(
                    "locale/locale_fr.ftl",
                    LocaleFormat,
                    (),
                    &mut progress_counter,
                    &storage,
                )
            },
        ));
        self.progress_counter = Some(progress_counter);
    }

    fn update(&mut self, world: &mut World) -> SimpleTrans<'a, 'b> {
        // Check if the locale has been loaded.
        if self.progress_counter.as_ref().unwrap().is_complete() {
            let store = world.read_resource::<AssetStorage<Locale>>();
            for h in [&self.handle_en, &self.handle_fr].iter() {
                if let Some(locale) = h.as_ref().and_then(|h| store.get(h)) {
                    println!("{}", locale.bundle.format("hello", None).unwrap().0);
                    println!("{}", locale.bundle.format("bye", None).unwrap().0);
                }
            }
            Trans::Quit
        } else {
            Trans::None
        }
    }
}

fn main() -> Result<(), Error> {
    amethyst::start_logger(Default::default());

    let resources_directory = format!("{}/examples/assets", application_root_dir());

    let game_data = GameDataBuilder::default().with(Processor::<Locale>::new(), "proc", &[]);

    let mut game = Application::build(resources_directory)?
        .with_state((), Example::new())?
        .build(game_data)?;

    game.run();
    Ok(())
}
