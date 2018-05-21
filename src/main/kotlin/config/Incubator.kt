package config

import com.natpryce.konfig.PropertyGroup
import com.natpryce.konfig.getValue
import com.natpryce.konfig.stringType

object incubator : PropertyGroup(){
    val apk by stringType
    val androidSdkPath by stringType

    // IC3 (icclinking)result of <apk>
    val apkIC3File by stringType
    val output by stringType
}