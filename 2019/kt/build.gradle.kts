val arrowVersion = "0.10.3"

plugins {
    id("org.jetbrains.kotlin.jvm") version "1.3.60"
    application
}

repositories {
    jcenter()
    maven("https://dl.bintray.com/arrow-kt/arrow-kt/")
}

dependencies {
    implementation("org.jetbrains.kotlin:kotlin-stdlib-jdk8")
    implementation("io.arrow-kt:arrow-core:$arrowVersion")
}

application {
    mainClassName = "moe.kageru.aoc.DayXXKt"
}
