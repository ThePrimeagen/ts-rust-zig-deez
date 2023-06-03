plugins {
    kotlin("jvm")
    id("org.jlleitschuh.gradle.ktlint")
    application
}

group = "dev.hermannm.monkeylang.jvm"
version = "0.1.0"

repositories {
    mavenCentral()
}

dependencies {
    implementation(project(":monkey-common"))
}

application {
    mainClass.set("dev.hermannm.monkeylang.jvm.JvmMainKt")
}

// Configures stdin for application
tasks.getByName("run", JavaExec::class) {
    standardInput = System.`in`
}