import traceback
from typing import Union, Tuple

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


@app.route('/<action>/axes_permutation', methods=['POST'])
def axes_permutation(action: str) -> Union[Response, str]:
    try:
        return jsonify(_prepare_transformation_recipe(request.json['eqn'],
                                                      action, ()).axes_permutation)
    except Exception as err:
        print(f'unexpected {err}, {type(err)}')
        j = f'{err}'
    return j


@app.route('/<action>/added_axes', methods=['POST'])
def added_axes(action: str) -> Union[Response, str]:
    try:
        return jsonify(_prepare_transformation_recipe(request.json['eqn'],
                                                      action, ()).added_axes)
    except Exception as err:
        print(f'unexpected {err}, {type(err)}')
        j = f'{err}'
    return j


@app.route('/<action>/output_composite_axes', methods=['POST'])
def output_composite_axes(action: str) -> Union[Response, str]:
    try:
        return jsonify(_prepare_transformation_recipe(request.json['eqn'],
                                                      action, ()).output_composite_axes)
    except Exception as err:
        print(f'unexpected {err}, {type(err)}')
        j = f'{err}'
    return j


@app.route('/<action>/ellipsis_position_in_lhs', methods=['POST'])
def ellipsis_position_in_lhs(action: str) -> Union[Response, str]:
    try:
        res = _prepare_transformation_recipe(
            request.json['eqn'],
            action, ()).ellipsis_position_in_lhs
        if res == 10000:
            res = None
        return jsonify(res)
    except Exception as err:
        print(f'unexpected {err}, {type(err)}')
        j = f'{err}'
    return j


# TODO: allow other reductions than max
@app.route('/<action>/elementary_axes_lengths', methods=['POST'])
def elementary_axes_lengths(action: str) -> Union[Response, str]:
    try:
        eqn: str = request.json['eqn']
        axes_lengths: Tuple[tuple, ...] = request.json['axes_lengths']
        axes_lengths = tuple([tuple(x) for x in axes_lengths])
        res_tmp = _prepare_transformation_recipe(eqn,
                                                 'max',
                                                 axes_lengths)
        res = res_tmp.elementary_axes_lengths
        res_may = []
        for x in res:
            res_may += [None] if x == -999999 else [x]
        return jsonify(res_may)
    except Exception as err:
        traceback.print_exc()
        print(f'unexpected {err}, {type(err)}')
        j = f'{err}'
    return j


@app.route('/<action>/input_composite_axes', methods=['POST'])
def input_composite_axes(action: str) -> Union[Response, str]:
    try:
        axes_lengths: Tuple[tuple, ...] = request.json['axes_lengths']
        axes_lengths = tuple([tuple(x) for x in axes_lengths])
        return jsonify(_prepare_transformation_recipe(request.json['eqn'],
                                                      action, axes_lengths).input_composite_axes)
    except Exception as err:
        print(f'unexpected {err}, {type(err)}')
        j = f'{err}'
    return j


@app.route('/<action>/reduced_elementary_axes', methods=['POST'])
def reduced_elementary_axes(action: str) -> Union[Response, str]:
    try:
        return jsonify(_prepare_transformation_recipe(request.json['eqn'],
                                                      action, ()).reduced_elementary_axes)
    except Exception as err:
        print(f'unexpected {err}, {type(err)}')
        j = f'{err}'
    return j

# init_shapes, reduced_axes, axes_reordering, added_axes, final_shapes
# _reconstruct_from_shape(recipe, backend.shape(tensor))
