plugins {
    kotlin("multiplatform") version "1.8.20"
    id("org.jmailen.kotlinter")
    id("org.jetbrains.compose").version("1.4.0")
}

version = "0.1.0"

repositories {
    mavenCentral()
    maven("https://maven.pkg.jetbrains.space/public/p/compose/dev")
    google()
}

kotlin {
    js(IR) {
        useCommonJs()
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
                implementation(npm("monaco-editor", "0.38.0"))
                implementation(npm("css-loader", "6.8.1"))
                implementation(npm("style-loader", "3.3.3"))
            }
        }
        val jsTest by getting {
            kotlin.setSrcDirs(listOf("test"))
            resources.setSrcDirs(listOf("test-resources"))
        }
    }
}
