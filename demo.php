<?php

require_once './vendor/autoload.php';

use glaryjoker\captcha\Captcha;

$a = new Captcha([
    'way' => 1
]);
$a->output();

