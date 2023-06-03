plugins {
    kotlin("multiplatform") version "1.8.10"
    id("org.jlleitschuh.gradle.ktlint") version "11.3.2"
}

group = "dev.hermannm.monkeylang"
version = "0.1.0"

repositories {
    mavenCentral()
}

kotlin {
    // This module doesn't have any source files to compile, but we still need to define a target
    // in the parent module because the compiler complains otherwise.
    targets {
        jvm()
        js(IR) {
            browser()
            nodejs()
        }
    }
}
