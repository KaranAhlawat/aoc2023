plugins {
    kotlin("jvm") version "1.9.20"
    application
}

application {
    if (hasProperty("day")) {
        mainClass.set("Day${property("day")}Kt")
    }
}

group = "io.ka"

version = "0.0.1"

repositories { mavenCentral() }

kotlin { jvmToolchain(21) }
