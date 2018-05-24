package config

import com.natpryce.konfig.PropertyGroup
import com.natpryce.konfig.enumType
import com.natpryce.konfig.getValue
import com.natpryce.konfig.stringType


object incubator : PropertyGroup() {
    val apk by stringType
    val output by stringType
    val mode by enumType(mapOf("ACTIVITY" to Mode.ACTIVITY, "SIMPLE" to Mode.SIMPLE, "FULL" to Mode.FULL))
    val ignoreApps by stringType
}