#!/usr/bin/env zx
import "fs";
import "path";
import { chalk } from "zx";

$.verbose = false;

cd("..");
await $`mkdir -p bin`;
await $`mkdir -p out`;
const data = {};
let total = 0;

const paths = await globby("../day*", {
  expandDirectories: true,
  onlyFiles: false,
});

for (const path of paths) {
  const name = path.split("/")[1];
  await $`go build -o bin/${name}_bin ./${name}`;

  await $`hyperfine --warmup 10 --export-json out/${name}.json bin/${name}_bin`;
  const dayData = fs.readJSONSync(`../out/${name}.json`);
  data[name] = dayData.results[0];
  delete data[name]["times"];
  delete data[name]["exit_codes"];
  const mean = (data[name]["mean"] * 1000)
  total += mean
  console.log(chalk.yellow(`${name}: ${mean.toFixed(2)} ms`));
}

console.log(chalk.green(`Total: ${total.toFixed(2)} ms`));

fs.writeFileSync("../out/results.json", JSON.stringify(data));

// Clean up
await $`rm -rf ./bin`;

console.log(chalk.green("All JSON results in ../out"));
