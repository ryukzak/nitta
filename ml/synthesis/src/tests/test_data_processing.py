import pandas as pd
from components.data_processing.feature_engineering import df_to_model_columns


def test_df_to_model_columns_conversion_introduces_columns():
    df_inp = pd.DataFrame({"a": [1, 2, 3], "b": [4, 5, 6]})
    df_out_expected = pd.DataFrame({"a": [1, 2, 3], "b": [4, 5, 6], "c": [0, 0, 0]})
    assert df_to_model_columns(df_inp, model_columns=["a", "b", "c"]).equals(df_out_expected)


def test_df_to_model_columns_conversion_removes_columns():
    df_inp = pd.DataFrame({"a": [1, 2, 3], "b": [4, 5, 6], "c": [0, 0, 0]})
    df_out_expected = pd.DataFrame({"a": [1, 2, 3], "b": [4, 5, 6]})
    assert df_to_model_columns(df_inp, model_columns=["a", "b"]).equals(df_out_expected)


def test_df_to_model_columns_conversion_reorders_columns():
    df_inp = pd.DataFrame({"a": [1, 2, 3], "b": [4, 5, 6], "c": [0, 0, 0]})
    df_out_expected = pd.DataFrame({"a": [1, 2, 3], "c": [0, 0, 0], "b": [4, 5, 6]})
    assert df_to_model_columns(df_inp, model_columns=["a", "c", "b"]).equals(df_out_expected)


def test_df_to_model_columns_conversion_fills_missing_data():
    df_inp = pd.DataFrame({"a": [1, 2, 3], "b": [4, pd.NA, pd.NA], "c": [pd.NA, pd.NA, 1]})
    df_out_expected = pd.DataFrame({"a": [1, 2, 3], "b": [4, 0, 0]})
    assert df_to_model_columns(df_inp, model_columns=["a", "b"]).equals(df_out_expected)
