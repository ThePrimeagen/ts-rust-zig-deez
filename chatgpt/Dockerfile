FROM rust:1.69

WORKDIR /deez

# Build dummy Cargo project so that crates.io index gets cached
COPY Cargo.toml Cargo.toml
COPY src/dummy-for-docker.rs src/main.rs
RUN cargo install --path .
RUN cargo run

# Build actual project
COPY . .
RUN cargo install --path .

# Run test
CMD [ "./test.sh" ]
