const monkey = import("./dist/monkey_web");

const throttle = function(fn, ms, scope) {
  var wait = false;
  return function() {
  var t = scope || this;
  var a = arguments;
  if (!wait) {
    wait = true;
    setTimeout(function() {
    fn.apply(t, a)
    wait = false;
    }, ms);
  }
  };
};

monkey.then(monkey => {
  const input = document.getElementById("input");
  const output = document.getElementById("output");

  input.addEventListener("input", throttle(function(e) {
    try {
      let result = monkey.evaluate(e.target.value);
      output.style.color = '#333';
      output.innerText = JSON.stringify(result.value, null, 2); // unwrap
    } catch(error) {
      output.style.color = '#CC0000';
      output.innerText = error;
    }
  }, 400, this));
});
