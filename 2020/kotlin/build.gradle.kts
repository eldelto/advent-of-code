plugins {
  kotlin("jvm") version "1.4.20"
}

group = "org.example"
version = "1.0-SNAPSHOT"

repositories {
  mavenCentral()
}

dependencies {
  implementation(kotlin("stdlib"))
  testImplementation("org.junit.jupiter:junit-jupiter-api:5.6.3")
  testRuntimeOnly("org.junit.jupiter:junit-jupiter-engine:5.6.3")
}

tasks.withType<Test> {
  useJUnitPlatform()
}
