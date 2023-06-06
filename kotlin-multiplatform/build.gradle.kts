plugins {
    kotlin("multiplatform") version "1.8.20"
    id("org.jmailen.kotlinter") version "3.15.0"
}

group = "dev.hermannm"
version = "0.1.0"

repositories {
    mavenCentral()
}

kotlin {
    jvm {
        jvmToolchain(11)
        withJava()
        testRuns["test"].executionTask.configure {
            useJUnitPlatform()
            testLogging {
                showStandardStreams = true
                events("passed", "skipped", "failed")
            }
        }
    }

    js(IR) {
        browser()
    }

    sourceSets {
        val commonMain by getting {
            kotlin.setSrcDirs(listOf("src"))
            resources.setSrcDirs(listOf("resources"))
        }
        val commonTest by getting {
            kotlin.setSrcDirs(listOf("test"))
            resources.setSrcDirs(listOf("test-resources"))

            dependencies {
                implementation(kotlin("test"))
            }
        }
    }
}
