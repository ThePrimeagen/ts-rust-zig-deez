import org.jetbrains.kotlin.konan.target.HostManager

plugins {
    kotlin("multiplatform")
    id("org.jlleitschuh.gradle.ktlint")
    id("org.jetbrains.compose")
}

group = "dev.hermannm.monkeylang.jvm"
version = "0.1.0"

repositories {
    google()
    mavenCentral()
    maven("https://maven.pkg.jetbrains.space/public/p/compose/dev")
}

kotlin {
    js(IR) {
        browser()
        binaries.executable()
    }
    sourceSets {
        val jsMain by getting {
            dependencies {
                implementation(project(":monkey-common"))
                implementation(compose.web.core)
                implementation(compose.runtime)
            }
        }
    }
}
