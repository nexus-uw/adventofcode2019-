const { readFileSync } = require("fs");

const input = readFileSync(__dirname + "/input").toString();

const picWidth = 25;
const picHeight = 6;
const picSize = picHeight * picWidth;

const pictures = input.split("").reduce((pics, pixel, index) => {
  if (index % picSize === 0) {
    pics.push([]);
  }
  pics[pics.length - 1].push(parseInt(pixel));

  return pics;
}, []);
function getCount(pic, val) {
  return pic.filter(p => p === val).length;
}
const min0Picture = pictures.reduce((min, picture) => {
  if (min === null) {
    return picture;
  }

  if (getCount(min, 0) < getCount(picture, 0)) {
    return min;
  } else {
    return picture;
  }
}, null);
const oneCountInMin0Picture = getCount(min0Picture, 1);
const twoCountInMin0Picture = getCount(min0Picture, 2);

console.log("part 1", oneCountInMin0Picture * twoCountInMin0Picture);
const finalPic = [];
for (let i = 0; i < picSize; i++) {
  let pixel = null;
  for (let x = 0; x < pictures.length; x++) {
    if (pixel === null) {
      pixel = pictures[x][i];
    } else if (pixel === 2 && pictures[x][i] !== 2) {
      //		// "0 is black, 1 is white, and 2 is transparent.""

      pixel = pictures[x][i];
    }
  }
  finalPic.push(pixel);
}
console.log("part 2");

for (let x = 0; x < picHeight; x++) {
  console.log(
    finalPic
      .slice(x * picWidth, (x + 1) * picWidth)
      .map(x => (x === 0 ? " " : "#"))
      .join("")
  );
}
