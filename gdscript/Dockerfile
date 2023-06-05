FROM debian:latest

WORKDIR /deez

RUN apt-get update && \
    apt-get -qqy upgrade && \
    apt-get -qqy install wget unzip make

RUN wget https://downloads.tuxfamily.org/godotengine/3.5.2/Godot_v3.5.2-stable_linux_headless.64.zip
RUN unzip Godot_v*.zip
RUN mv Godot_v*.64 /usr/bin/godot

CMD make ready

