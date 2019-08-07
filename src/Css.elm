module Css exposing (css)


css : String
css =
    """
.calculator {
  border-radius: 5px;
  position: absolute;
  top: 50%;
  left: 50%;
  transform: translate(-50%, -50%);
  width: 400px;
}
.display {
  font-size: 5rem;
  height: 80px;
  background-color: #252525;
  color: #fff;
  text-align: right;
  padding-right: 20px;
}
button {
  height: 60px;
  border-radius: 3px;
  background-color: transparent;
  font-size: 2rem;
  color: #333;
}
.buttons {
  display: grid;
  grid-template-columns: repeat(4, 1fr);
}
.equal {
  height: 100%;
  grid-area: 2 / 4 / 6 / 5;
}
"""
