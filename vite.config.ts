import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'

// https://vite.dev/config/
export default defineConfig({
  plugins: [react()],
  // O site é publicado no GitHub Pages em https://eullerm.github.io/GameOfLife/
  base: '/GameOfLife/',
})
