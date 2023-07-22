import { defineConfig } from 'vite'
import elmPlugin from 'vite-plugin-elm'

export default defineConfig({
    plugins: [elmPlugin({
        optimize: false, // no `--optimize` option when using elm-optimize-level-2
        nodeElmCompilerOptions: {
            pathToElm: process.env.NODE_ENV === 'production' ? 'node_modules/elm-optimize-level-2/bin/elm-optimize-level-2' : undefined
        }
    })]
})


