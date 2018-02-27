<?php

// autoload_static.php @generated by Composer

namespace Composer\Autoload;

class ComposerStaticInitd148637dbcfae901328573ccf76f12f7
{
    public static $prefixLengthsPsr4 = array (
        'g' => 
        array (
            'glaryjoker\\captcha\\' => 19,
        ),
    );

    public static $prefixDirsPsr4 = array (
        'glaryjoker\\captcha\\' => 
        array (
            0 => __DIR__ . '/../..' . '/src',
        ),
    );

    public static function getInitializer(ClassLoader $loader)
    {
        return \Closure::bind(function () use ($loader) {
            $loader->prefixLengthsPsr4 = ComposerStaticInitd148637dbcfae901328573ccf76f12f7::$prefixLengthsPsr4;
            $loader->prefixDirsPsr4 = ComposerStaticInitd148637dbcfae901328573ccf76f12f7::$prefixDirsPsr4;

        }, null, ClassLoader::class);
    }
}
