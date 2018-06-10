use futures::future;
use futures::{Future, Stream};
use hyper::{Client, Error, Uri};
use serde_json;
use std::fmt;
use tokio_core::reactor::Core;

#[derive(Serialize, Deserialize)]
struct Source {
    pub name: String,
    pub url: String,
}

#[derive(Serialize, Deserialize)]
struct Sources {
    pub sources: Vec<Source>,
}

pub struct Fetcher {
    sources: Sources,
}

impl fmt::Display for Fetcher {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for source in self.sources.sources.iter() {
            writeln!(f, "{}", source.name).unwrap();
        }
        Ok(())
    }
}

impl Fetcher {
    pub fn new() -> Self {
        let data: &'static str = include_str!("data.json");
        let sources: Sources = serde_json::from_str(data).unwrap();
        Fetcher { sources: sources }
    }

    pub fn fetch(&self, name: &str) -> Option<String> {
        match self.sources.sources.iter().find(|ref s| s.name == name) {
            Some(ref source) => match self.do_fetch(&source.url) {
                Some(body) => Some(body),
                None => None,
            },
            None => None,
        }
    }

    fn do_fetch(&self, url: &str) -> Option<String> {
        let mut core = Core::new().unwrap();
        let client = Client::new(&core.handle());

        let uri: Uri = url.parse().unwrap();
        let work = client.get(uri).and_then(|res| {
            res.body()
                .fold(Vec::new(), |mut v, chunk| {
                    v.extend(&chunk[..]);
                    future::ok::<_, Error>(v)
                })
                .and_then(|chunks| {
                    let s = String::from_utf8(chunks).unwrap();
                    future::ok::<_, Error>(s)
                })
        });

        let res = core.run(work).unwrap();
        Some(res)
    }
}
