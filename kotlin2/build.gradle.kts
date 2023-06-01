plugins {
    kotlin("jvm") version "1.9.0-Beta"
    application
    id("org.jlleitschuh.gradle.ktlint") version "11.3.2"
}

group = "dev.hermannm"
version = "0.1.0"

sourceSets {
    main.get().kotlin.setSrcDirs(listOf("src"))
    main.get().resources.setSrcDirs(listOf("resources"))
    test.get().kotlin.setSrcDirs(listOf("tests"))
}

application {
    mainClass.set("dev.hermannm.monkeylang.ReplKt")
}

// Configures stdin for application
tasks.getByName("run", JavaExec::class) {
    standardInput = System.`in`
}

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
