# example: run this with "python grader_filename.py your_code_filename"
# this is an auto-generated file for general student testing

import sys
import subprocess
import os
from difflib import Differ
if __name__ == "__main__":
    fn = sys.argv[1]
    tmp_fn = "tmp.rkt"
    feedback_fn = "feedback.txt"
    run_cmd = "julia"
    tests = """

let
  testnum = 0
  global tnum
  tnum() = testnum += 1
end

push!(LOAD_PATH, pwd())

using Lexer
using Error

function testNum(num)
  return string(num) * ". "
end

function lexParse(str)
  CITypes.parse(Lexer.lex(str))
end

function parseInter(str)
  CITypes.type_of_expr(lexParse(str))
end

function removeNL(str)
  replace(string(str), "\n" => "")
end

function testerr(f, param, num)
  try
    println(testNum(num) *  removeNL(f(param)))
  catch Y
    if (typeof(Y) != Error.LispError)
      println(testNum(num) * removeNL(Y))
    else
      println(testNum(num) * "Error")
    end
  end
end


testerr(parseInter, "true", tnum())
testerr(parseInter, "4", tnum())
testerr(parseInter, "(+ 1 2)", tnum())

testerr(parseInter, "(iszero nempty)", tnum())
testerr(parseInter, "(ifb false (ncons 1 nempty) nempty)", tnum())

testerr(parseInter, "(with x 3 ( + 1 x))", tnum())
testerr(parseInter, "(lambda x : number false)", tnum())
testerr(parseInter, "((lambda x : number false) false)", tnum())

testerr(parseInter, "nempty", tnum())
testerr(parseInter, "(nisempty false)", tnum())
testerr(parseInter, "(nfirst (ncons 1 nempty))", tnum())
testerr(parseInter, "(nrest nempty)", tnum())
"""
    tests_info = """1. 1 point. true
2. 1 point. 4
3. 1 point. (+ 1 2)
4. 1 point. (iszero nempty)
5. 1 point. (ifb false (ncons 1 nempty) nempty)
6. 1 point. (with x 3 ( + 1 x))
7. 1 point. (lambda x : number false)
8. 1 point. ((lambda x : number false) false)
9. 1 point. nempty
10. 1 point. (nisempty false)
11. 1 point. (nfirst (ncons 1 nempty))
12. 1 point. (nrest nempty)
"""
    correctoutput = """1. Main.CITypes.BoolType()
2. Main.CITypes.NumType()
3. Main.CITypes.NumType()
4. Error
5. Main.CITypes.NListType()
6. Main.CITypes.NumType()
7. Main.CITypes.FuncType(Main.CITypes.NumType(), Main.CITypes.BoolType())
8. Error
9. Main.CITypes.NListType()
10. Error
11. Main.CITypes.NumType()
12. Main.CITypes.NListType()
"""
    grade = 0
    total_possible = 0
    with open(fn, "r") as f:
        with open(tmp_fn, "w") as w:
            w.write(f.read())
            w.write(tests)
    cmd = [run_cmd, tmp_fn]
    process = subprocess.Popen(cmd, stdout=subprocess.PIPE)
    studentoutput, err = process.communicate()
    studentoutput = studentoutput.decode('utf-8')
    comparison = "".join(Differ().compare(correctoutput.splitlines(1), studentoutput.splitlines(1)))
    error_line_nos = []
    extra_line_nos = []
    q_line_nos = []
    for count, i in enumerate(comparison.splitlines()):
        if "-" == i[0]:
            error_line_nos.append(count)
        elif "+" == i[0]:
            extra_line_nos.append(count)
        elif "?" == i[0]:
            q_line_nos.append(count)
    failed_tests_line_nos = []
    for x in error_line_nos:
        numextralines = len([y for y in extra_line_nos if y < x])
        numqlines = len([z for z in q_line_nos if z < x])
        failed_tests_line_nos.append(x - numextralines - numqlines)
    with open(feedback_fn, "w") as feedback_file:
        feedback_file.write("        Correct output:\n")
        feedback_file.write(str(correctoutput))
        feedback_file.write("\n        Your output:\n")
        feedback_file.write(str(studentoutput))
        feedback_file.write("\n        Failed tests:\n")
        for count, l in enumerate(tests_info.splitlines(1)):
            points = int(l.split()[1])
            if count in failed_tests_line_nos:
                total_possible += points
                feedback_file.write(l)
            else:
                total_possible += points
                grade += points
        feedback_file.write("\n        Grade:\n" + str(grade) + " out of " + str(total_possible))
    os.remove(tmp_fn)
    print("See feedback file: " + feedback_fn)
