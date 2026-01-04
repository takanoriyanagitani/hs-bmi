#!/bin/sh

ex_ok() {

	echo "Running ex_ok (Valid input)..."

	printf 'height:1.75\thunit:M\tweight:70\twunit:Kg\n' | ./hs-bmi

}

ex_ng_range() {

	echo "Running ex_ng_range (Too tall)..."

	printf 'height:4.0\thunit:M\tweight:70.0\twunit:Kg\n' | ./hs-bmi

}

ex_ng_unit() {

	echo "Running ex_ng_unit (Invalid unit)..."

	printf 'height:175\thunit:unknown\tweight:70.0\twunit:Kg\n' | ./hs-bmi

}

ex_ng_label() {

	echo "Running ex_ng_label (Missing label)..."

	printf 'h:1.75\thunit:M\tweight:70.0\twunit:Kg\n' | ./hs-bmi

}

ex_ok_2() {

	echo "Running ex_ok_2 (Valid input, grams and cm)..."

	printf 'height:175\thunit:cm\tweight:70000\twunit:g\n' | ./hs-bmi

}

ex_ok_low() {

	echo "Running ex_ok_low (Low BMI)..."

	printf 'height:1.8\thunit:M\tweight:50\twunit:Kg\n' | ./hs-bmi

}

ex_ok_high() {

	echo "Running ex_ok_high (High BMI)..."

	printf 'height:1.6\thunit:M\tweight:80\twunit:Kg\n' | ./hs-bmi

}

ex_ng_zero() {

	echo "Running ex_ng_zero (Zero value)..."

	printf 'height:1.75\thunit:M\tweight:0\twunit:Kg\n' | ./hs-bmi

}

run_all() {

	ex_ok

	ex_ok_2

	ex_ok_low

	ex_ok_high

	ex_ng_range

	ex_ng_unit

	ex_ng_label

	ex_ng_zero

}

if [ $# -eq 0 ]; then

	run_all

else

	for arg in "$@"; do

		case "$arg" in

		"ok") ex_ok ;;

		"ok2") ex_ok_2 ;;

		"low") ex_ok_low ;;

		"high") ex_ok_high ;;

		"range") ex_ng_range ;;

		"unit") ex_ng_unit ;;

		"label") ex_ng_label ;;

		"zero") ex_ng_zero ;;

		*) echo "Unknown example: $arg" ;;

		esac

	done

fi
