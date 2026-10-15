import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'

// https://vite.dev/config/
export default defineConfig({
  plugins: [react()],
  // O site é publicado no GitHub Pages em https://eullerm.github.io/GameOfLife/
  base: '/GameOfLife/',
  server: {
    proxy: {
      // O backend Haskell (npm run api) roda na porta 8787
      // (a porta 8080 costuma estar ocupada por outros serviços, ex. wslrelay)
      '/api': 'http://127.0.0.1:8787',
    },
  },
})
