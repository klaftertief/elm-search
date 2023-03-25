const esbuild = require('esbuild');
const ElmPlugin = require('esbuild-plugin-elm');

esbuild.build({
    entryPoints: ['../generator/src/generator.js'],
    bundle: true,
    outdir: '../dist',
    platform: 'node',
    plugins: [
        ElmPlugin({optimize: false, debug: true }),
    ],
}).catch(e => (console.error(e), process.exit(1)))
