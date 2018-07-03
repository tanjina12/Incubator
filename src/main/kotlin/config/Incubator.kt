package config

import com.natpryce.konfig.*


object incubator : PropertyGroup() {
    val apk by stringType
    val output by stringType
//    val mode by enumType(mapOf("ACTIVITY" to Mode.ACTIVITY, "SIMPLE" to Mode.SIMPLE, "FULL" to Mode.FULL))
    val ignoreApps by stringType
//    val parseInnerClassAdOuterClass by booleanType
}