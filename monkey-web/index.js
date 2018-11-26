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
      output.innerText = result;
    } catch(error) {
      output.innerText = error;
    }
  }, 400, this));
});
