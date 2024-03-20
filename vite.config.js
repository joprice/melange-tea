import { nodeResolve } from "@rollup/plugin-node-resolve";
import path from 'path'

const parent = path.resolve(__dirname, '../');
console.log(parent)
export default {
  build: {
    outDir: "./dist",
  },
  plugins: [nodeResolve()],
  resolve: {
    alias: {
      '/@': parent
    },
  },
};
