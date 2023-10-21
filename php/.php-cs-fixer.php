<?php

$finder = PhpCsFixer\Finder::create()
    ->exclude('vendor')
    ->in(__DIR__);

$config = new PhpCsFixer\Config();
return $config->setRules([
    '@PSR12' => true,
    'curly_braces_position' => [
        'functions_opening_brace' => 'same_line',
        'classes_opening_brace' => 'same_line',
    ],
])
    ->setFinder($finder);
