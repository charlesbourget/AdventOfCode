#!/usr/bin/env zx
import { argv } from "process";

const number = process.argv[3];

let current_value = (
  await $`grep -E -o -e 'message=\\d+' ../README.md`
).toString();

const new_value = parseInt(current_value.split("=")[1]) + parseInt(number);

await $`perl -i -pe"s/message=\\d+/message=${new_value}/g" ../README.md`;
