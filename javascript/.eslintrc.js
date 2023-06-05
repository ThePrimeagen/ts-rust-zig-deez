module.exports = {
    plugins: ["jest"],
    env: {
        browser: true,
        commonjs: true,
        es2021: true,
        "jest/globals": true,
    },
    extends: "eslint:recommended",
    overrides: [
    ],
    parserOptions: {
        ecmaVersion: "latest",
    },
    rules: {
        indent: [
            "error",
            4,
        ],
        "linebreak-style": [
            "error",
            "unix",
        ],
        quotes: [
            "error",
            "double",
        ],
        // always semi-colons
        semi: [
            "error",
            "always",
        ],
        // comma dangle on multiline
        "comma-dangle": [
            "error",
            "always-multiline",
        ],
        // properties shouldn't be quoted unless necessary
        "quote-props": [
            "error",
            "as-needed",
        ],
    },
};
