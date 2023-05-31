plugins {
    kotlin("jvm") version "1.9.0-Beta"
    id("org.jlleitschuh.gradle.ktlint") version "11.3.2"
}

group = "dev.hermannm"
version = "0.1.0"

repositories {
    mavenCentral()
}

dependencies {
    testImplementation(kotlin("test"))
}

tasks.test {
    testLogging {
        showStandardStreams = true
        events("passed", "skipped", "failed")
    }

    useJUnitPlatform()
}

kotlin {
    jvmToolchain(11)
}
