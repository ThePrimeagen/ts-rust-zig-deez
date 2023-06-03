plugins {
    kotlin("multiplatform")
    id("org.jlleitschuh.gradle.ktlint")
}

group = "dev.hermannm.monkeylang"
version = "0.1.0"

repositories {
    mavenCentral()
}

kotlin {
    targets {
        jvm()
        js(IR) {
            browser()
            nodejs()
        }
    }

    sourceSets {
        val commonMain by getting {
            dependencies {
                implementation(kotlin("stdlib-common"))
            }
        }
        val commonTest by getting {
            dependencies {
                implementation(kotlin("test"))
            }
        }
    }
}
