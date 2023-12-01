package testutils

import (
	"reflect"
	"strings"
	"testing"
)

func AssertEquals(t *testing.T, expected, actual any, title string) {
	t.Helper()

	if !reflect.DeepEqual(expected, actual) {
		t.Errorf("%s should be\n'%v'\nbut was\n'%v'", title, expected, actual)
	}
}

func AssertNotEquals(t *testing.T, expected, actual any, title string) {
	t.Helper()

	if reflect.DeepEqual(expected, actual) {
		t.Errorf("%s should not be\n'%v'\nbut was\n'%v'", title, expected, actual)
	}
}

func AssertContains(t *testing.T, expected any, testee []any, title string) {
	t.Helper()

	for _, actual := range testee {
		if reflect.DeepEqual(expected, actual) {
			return
		}
	}

	t.Errorf("%s did not contain a value '%v': %v", title, expected, testee)
}

func AssertStringContains(t *testing.T, expected, testee, title string) {
	t.Helper()

	if !strings.Contains(testee, expected) {
		t.Errorf("%s did not contain the substring '%v': %v", title, expected, testee)
	}
}

func AssertNoError(t *testing.T, err error, title string) {
	t.Helper()

	if err != nil {
		t.Errorf("%s should not return an error but returned '%v'", title, err)
	}
}
