import pytest

from components.utils.string import snake_to_lower_camel_case


@pytest.mark.parametrize(
    ("inp", "out_expected"),
    [
        ("", ""),
        ("test", "test"),
        ("test_case_1", "testCase1"),
        ("test_case_1_2", "testCase12"),
        ("_test_case_1_2", "testCase12"),
        ("__test_case_1_2", "testCase12"),
        ("__test_case_1_2_", "testCase12"),
        ("__test_case_1_2__", "testCase12"),
        ("test_multi_word_test_test", "testMultiWordTestTest"),
        ("test_multi_word_test_test_", "testMultiWordTestTest"),
        ("test_multi_word_test_test__", "testMultiWordTestTest"),
        ("test_multi_word_test_test__12", "testMultiWordTestTest12"),
        ("test_multi_word_test_test__1_2", "testMultiWordTestTest12"),
        ("test_multi_word_test_test__1__2", "testMultiWordTestTest12"),
        ("test_multi_word_test_test__1__2_", "testMultiWordTestTest12"),
        ("test_multi_word_test_test__1__2__", "testMultiWordTestTest12"),
        ("test18", "test18"),
        ("test_multi18", "testMulti18"),
        ("_", ""),
        ("__", ""),
    ],
)
def test_snake_to_lower_camel_case(inp, out_expected):
    assert snake_to_lower_camel_case(inp) == out_expected
