import { defineConfig } from 'vite'
import { resolve } from 'node:path'

const jsOutput = resolve('target/scala-3.8.3/scorry-fastopt.js')

export default defineConfig({
  server: {
    watch: {
      usePolling: true,
      interval: 500
    }
  },
  plugins: [
    {
      name: 'watch-scalajs',
      configureServer(server) {
        server.watcher.add(jsOutput)
        server.watcher.on('change', (file) => {
          if (file === jsOutput) {
            server.ws.send({ type: 'full-reload' })
          }
        })
      }
    }
  ]
})
