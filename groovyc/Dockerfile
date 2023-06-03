FROM ubuntu:latest
COPY . /groovybaby
WORKDIR /groovybaby
SHELL ["/bin/bash", "-c"]
RUN apt update
RUN apt install -y groovy make
RUN GROOVY_HOME=/usr/share/groovy make test
RUN GROOVY_HOME=/usr/share/groovy make repl
CMD ["java", "-cp", "\".://usr/share/groovy/lib/*\"", "repl"]
