const ALL_NUMBERS = ['0', '1', '2', '3', '4'];
/**
 *
 * @param {number} digit
 * @param {string[]} numbers
 */
function foo(numbers) {
  return numbers.reduce((lst, num) => {
    const remainingNumbers = numbers.filter(i => i != num);
    console.log(`num: ${num} remainingNumbers: ${remainingNumbers}`);
    if (remainingNumbers.length === 0) {
      return lst.concat([num]);
    } else {
      return lst.concat(foo(remainingNumbers).map(i => num + i));
    }
  }, []);
}

foo(ALL_NUMBERS).forEach(n => console.log(`${n},`));
