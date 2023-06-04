plugins {
    kotlin("multiplatform") version "1.8.20"
    id("org.jmailen.kotlinter")
    id("org.jetbrains.compose").version("1.4.0")
}

group = "dev.hermannm"
version = "0.1.0"

repositories {
    mavenCentral()
    maven("https://maven.pkg.jetbrains.space/public/p/compose/dev")
    google()
}

kotlin {
    js(IR) {
        browser()
        binaries.executable()
    }

    sourceSets {
        val jsMain by getting {
            kotlin.setSrcDirs(listOf("src"))
            resources.setSrcDirs(listOf("resources"))

            dependencies {
                implementation(project(":")) // Common root package
                implementation(compose.html.core)
            }
        }
        val jsTest by getting {
            kotlin.setSrcDirs(listOf("test"))
            resources.setSrcDirs(listOf("test-resources"))
        }
    }
}
