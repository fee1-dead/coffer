use std::io::{Cursor, Read};
use std::sync::Arc;

use dashmap::DashSet;
use tokio::join;
use tokio::sync::mpsc;
use tokio::task::JoinHandle;
use tokio_stream::StreamExt;
use tokio_stream::wrappers::UnboundedReceiverStream;
use zip::ZipArchive;
use zip::read::ZipFile;

pub type Error = Box<dyn std::error::Error + Send + Sync>;
pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug)]
pub struct ClassInfo {
    pub file_name: String,
    pub bytes: Vec<u8>,
}

struct MyZipFile<'a>(ZipFile<'a>);

unsafe impl<'a> Send for MyZipFile<'a> {}

pub async fn get_sample_name_bytes_async(distribution_size: u64) -> Result<Vec<ClassInfo>> {
    fn spawn(s: &'static str, client: Arc<reqwest::Client>, sender: Arc<mpsc::UnboundedSender<ClassInfo>>, distribution_size: u64, sizes: Arc<DashSet<u64>>) -> JoinHandle<Result<()>> {
        tokio::spawn(async move {
            let bytes = client.get(s).send().await?.bytes().await?;
            let bytes = bytes.as_ref();
            let mut zip = ZipArchive::new(Cursor::new(bytes))?;
            for i in 0..zip.len() {
                let mut file = MyZipFile(zip.by_index(i)?);
                let file_name = file.0.name().to_owned();
                let size = file.0.size() / distribution_size;
                if file.0.is_file() && file_name.ends_with(".class") && !sizes.contains(&size) {
                    sizes.insert(size);
                    let mut bytes = vec![];
                    file.0.read_to_end(&mut bytes)?;
                    drop(file);
                    sender.send(ClassInfo { file_name, bytes })?;
                }
            }
            Ok(())
        })
    }

    let guava = "https://repo1.maven.org/maven2/com/google/guava/guava/30.0-jre/guava-30.0-jre.jar";
    let okhttp = "https://repo1.maven.org/maven2/com/squareup/okhttp3/okhttp/4.10.0-RC1/okhttp-4.10.0-RC1.jar";
    let spark = "https://repo1.maven.org/maven2/org/apache/spark/spark-core_2.11/2.4.7/spark-core_2.11-2.4.7.jar";
    let zxing = "https://repo1.maven.org/maven2/com/google/zxing/core/3.4.1/core-3.4.1.jar";
    let guice = "https://repo1.maven.org/maven2/com/google/inject/guice/5.0.0-BETA-1/guice-5.0.0-BETA-1.jar";
    let junit = "https://repo1.maven.org/maven2/junit/junit/4.13.1/junit-4.13.1.jar";
    let kotlin = "https://repo1.maven.org/maven2/org/jetbrains/kotlin/kotlin-compiler/1.4.20-M1/kotlin-compiler-1.4.20-M1.jar";
    let scala = "https://repo1.maven.org/maven2/org/scala-lang/scala-compiler/2.13.3/scala-compiler-2.13.3.jar";
    let bitcoinj = "https://repo1.maven.org/maven2/org/bitcoinj/bitcoinj-core/0.15.8/bitcoinj-core-0.15.8.jar";

    let (sender, mut receiver) = mpsc::unbounded_channel();
    let sender = Arc::new(sender);
    let client = Arc::new(reqwest::Client::new());
    let sizes = Arc::new(DashSet::new());

    macro_rules! spawn_join_all {
        ($($i:ident),*) => {
            $(
                let $i = spawn($i, client.clone(), sender.clone(), distribution_size, sizes.clone());
            )*
            let ($($i),*) = join!($($i),*);
            $($i??;)*
        };
    }

    spawn_join_all!(guava, okhttp, spark, zxing, guice, junit, kotlin, scala, bitcoinj);
    receiver.close();
    Ok(UnboundedReceiverStream::new(receiver).collect().await)
}

pub fn get_sample_name_bytes(distribution_size: u64) -> Result<Vec<ClassInfo>> {
    tokio::runtime::Runtime::new().unwrap().block_on(get_sample_name_bytes_async(distribution_size))
}
