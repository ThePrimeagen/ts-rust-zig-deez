pluginManagement {
    repositories {
        google()
        gradlePluginPortal()
        mavenCentral()
        maven("https://maven.pkg.jetbrains.space/public/p/compose/dev")
    }

    plugins {
        kotlin("multiplatform").version("1.8.20")
        id("org.jetbrains.compose").version("1.3.1")
    }
}

rootProject.name = "monkeylang"
include("monkey-common")
include("monkey-jvm")
include("monkey-js")