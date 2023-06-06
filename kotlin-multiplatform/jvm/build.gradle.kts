plugins {
    kotlin("multiplatform") version "1.8.20"
    id("org.jmailen.kotlinter")
    application
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

    sourceSets {
        val jvmMain by getting {
            kotlin.setSrcDirs(listOf("src"))
            resources.setSrcDirs(listOf("resources"))

            dependencies {
                implementation(project(":")) // Common root package
            }
        }
        val jvmTest by getting {
            kotlin.setSrcDirs(listOf("test"))
            resources.setSrcDirs(listOf("test-resources"))
        }
    }
}

application {
    mainClass.set("dev.hermannm.monkeylang.jvm.ReplKt")
}

// From https://slack-chats.kotlinlang.org/t/522898/how-should-i-correctly-replace-application-mainclass-set-com
tasks.named<JavaExec>("run") {
    dependsOn(tasks.named<Jar>("jvmJar"))
    classpath(tasks.named<Jar>("jvmJar"))
    standardInput = System.`in`
}
