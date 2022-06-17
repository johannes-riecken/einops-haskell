from typing import Union

from einops.einops import _prepare_transformation_recipe
from flask import Flask, jsonify, request
from werkzeug.exceptions import HTTPException
from werkzeug.sansio.response import Response

app = Flask(__name__)


# inspired by https://flask.palletsprojects.com/en/2.1.x/errorhandling/
@app.errorhandler(HTTPException)
def handle_exception(e: HTTPException) -> Response:
    """Return string instead of HTML for HTTP errors."""
    # start with the correct headers and status code from the error
    response = e.get_response()
    # replace the body with JSON
    response.data = e
    response.content_type = "text/plain"
    return response


@app.route('/axes_permutation', methods=['POST'])
def axes_permutation() -> Union[Response, str]:
    try:
        return jsonify(_prepare_transformation_recipe(request.json['eqn'],
                                                      'rearrange', ()).axes_permutation)
    except Exception as err:
        print(f'unexpected {err}, {type(err)}')
        j = f'{err}'
    return j


@app.route('/added_axes', methods=['POST'])
def added_axes() -> Union[Response, str]:
    try:
        return jsonify(_prepare_transformation_recipe(request.json['eqn'],
                                                      'rearrange', ()).added_axes)
    except Exception as err:
        print(f'unexpected {err}, {type(err)}')
        j = f'{err}'
    return j


@app.route('/output_composite_axes', methods=['POST'])
def output_composite_axes() -> Union[Response, str]:
    try:
        return jsonify(_prepare_transformation_recipe(request.json['eqn'],
                                                      'rearrange', ()).output_composite_axes)
    except Exception as err:
        print(f'unexpected {err}, {type(err)}')
        j = f'{err}'
    return j


@app.route('/ellipsis_position_in_lhs', methods=['POST'])
def ellipsis_position_in_lhs() -> Union[Response, str]:
    try:
        res = _prepare_transformation_recipe(
            request.json['eqn'],
            'rearrange', ()).ellipsis_position_in_lhs
        if res == 10000:
            res = None
        return jsonify(res)
    except Exception as err:
        print(f'unexpected {err}, {type(err)}')
        j = f'{err}'
    return j


@app.route('/elementary_axes_lengths', methods=['POST'])
def elementary_axes_lengths() -> Union[Response, str]:
    try:
        res = _prepare_transformation_recipe(request.json['eqn'],
                                             'rearrange', ()).elementary_axes_lengths
        res_may = []
        for x in res:
            res_may += [None] if x == -999999 else [x]
        return jsonify(res_may)
    except Exception as err:
        print(f'unexpected {err}, {type(err)}')
        j = f'{err}'
    return j


@app.route('/input_composite_axes', methods=['POST'])
def input_composite_axes() -> Union[Response, str]:
    try:
        return jsonify(_prepare_transformation_recipe(request.json['eqn'],
                                                      'rearrange', ()).input_composite_axes)
    except Exception as err:
        print(f'unexpected {err}, {type(err)}')
        j = f'{err}'
    return j


@app.route('/reduced_elementary_axes', methods=['POST'])
def reduced_elementary_axes() -> Union[Response, str]:
    try:
        return jsonify(_prepare_transformation_recipe(request.json['eqn'],
                                                      'rearrange', ()).reduced_elementary_axes)
    except Exception as err:
        print(f'unexpected {err}, {type(err)}')
        j = f'{err}'
    return j
