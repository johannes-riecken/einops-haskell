from typing import Union, Tuple

from einops.einops import _prepare_transformation_recipe, _reconstruct_from_shape, EinopsError
from flask import Flask, jsonify, request
from werkzeug.exceptions import HTTPException
from werkzeug.sansio.response import Response
import numpy as np

ims = np.reshape(np.fromfile('/Users/rieckenj/repos/einops-haskell/out.bin', dtype='byte'), (6, 4, 4, 3))
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


# generate python BEGIN
@app.route('/<action>/added_axes', methods=['POST'])
def added_axes(action: str) -> Union[Response, str]:
    try:
        axes_lengths: Tuple[tuple, ...] = request.json['axes_lengths']
        axes_lengths = tuple(tuple(x) for x in axes_lengths)
        return jsonify(_prepare_transformation_recipe(request.json['eqn'],
                                                      'max' if action == 'reduce' else action, axes_lengths).added_axes)
    except EinopsError as err:
        print(f'unexpected {err}, {type(err)}')
        j = f'{err}'
    return j


@app.route('/<action>/axes_permutation', methods=['POST'])
def axes_permutation(action: str) -> Union[Response, str]:
    try:
        axes_lengths: Tuple[tuple, ...] = request.json['axes_lengths']
        axes_lengths = tuple(tuple(x) for x in axes_lengths)
        return jsonify(_prepare_transformation_recipe(request.json['eqn'],
                                                      'max' if action == 'reduce' else action,
                                                      axes_lengths).axes_permutation)
    except EinopsError as err:
        print(f'unexpected {err}, {type(err)}')
        j = f'{err}'
    return j


@app.route('/<action>/elementary_axes_lengths', methods=['POST'])
def elementary_axes_lengths(action: str) -> Union[Response, str]:
    try:
        axes_lengths: Tuple[tuple, ...] = request.json['axes_lengths']
        axes_lengths = tuple(tuple(x) for x in axes_lengths)
        res_tmp = _prepare_transformation_recipe(request.json['eqn'], 'max', axes_lengths)
        res = res_tmp.elementary_axes_lengths
        res_may = []
        for x in res:
            res_may += [None] if x == -999999 else [x]
        return jsonify(res_may)
    except EinopsError as err:
        print(f'unexpected {err}, {type(err)}')
        j = f'{err}'
    return j


@app.route('/<action>/ellipsis_position_in_lhs', methods=['POST'])
def ellipsis_position_in_lhs(action: str) -> Union[Response, str]:
    try:
        axes_lengths: Tuple[tuple, ...] = request.json['axes_lengths']
        axes_lengths = tuple(tuple(x) for x in axes_lengths)
        return jsonify(_prepare_transformation_recipe(request.json['eqn'],
                                                      'max' if action == 'reduce' else action,
                                                      axes_lengths).ellipsis_position_in_lhs)
    except EinopsError as err:
        print(f'unexpected {err}, {type(err)}')
        j = f'{err}'
    return j


@app.route('/<action>/input_composite_axes', methods=['POST'])
def input_composite_axes(action: str) -> Union[Response, str]:
    try:
        axes_lengths: Tuple[tuple, ...] = request.json['axes_lengths']
        axes_lengths = tuple(tuple(x) for x in axes_lengths)
        return jsonify(_prepare_transformation_recipe(request.json['eqn'],
                                                      'max' if action == 'reduce' else action,
                                                      axes_lengths).input_composite_axes)
    except EinopsError as err:
        print(f'unexpected {err}, {type(err)}')
        j = f'{err}'
    return j


@app.route('/<action>/output_composite_axes', methods=['POST'])
def output_composite_axes(action: str) -> Union[Response, str]:
    try:
        axes_lengths: Tuple[tuple, ...] = request.json['axes_lengths']
        axes_lengths = tuple(tuple(x) for x in axes_lengths)
        return jsonify(_prepare_transformation_recipe(request.json['eqn'],
                                                      'max' if action == 'reduce' else action,
                                                      axes_lengths).output_composite_axes)
    except EinopsError as err:
        print(f'unexpected {err}, {type(err)}')
        j = f'{err}'
    return j


@app.route('/<action>/reduced_elementary_axes', methods=['POST'])
def reduced_elementary_axes(action: str) -> Union[Response, str]:
    try:
        axes_lengths: Tuple[tuple, ...] = request.json['axes_lengths']
        axes_lengths = tuple(tuple(x) for x in axes_lengths)
        return jsonify(_prepare_transformation_recipe(request.json['eqn'],
                                                      'max' if action == 'reduce' else action,
                                                      axes_lengths).reduced_elementary_axes)
    except EinopsError as err:
        print(f'unexpected {err}, {type(err)}')
        j = f'{err}'
    return j


# generate python END


reconstruct_fields = {
    "init_shapes": 0,
    "reduced_axes": 1,
    "axes_reordering": 2,
    "added_axes_reconstruct": 3,
    "final_shapes": 4,
}


# _reconstruct_from_shape BEGIN
@app.route('/<action>/init_shapes', methods=['POST'])
def init_shapes(action: str) -> Union[Response, str]:
    try:
        axes_lengths: Tuple[tuple, ...] = request.json['axes_lengths']
        axes_lengths = tuple(tuple(x) for x in axes_lengths)
        recipe = _prepare_transformation_recipe(request.json['eqn'],
                                                'max' if action == 'reduce' else action,
                                                axes_lengths)
        return jsonify(_reconstruct_from_shape(recipe,
                                               ims.shape)[reconstruct_fields['init_shapes']])
    except EinopsError as err:
        print(f'unexpected {err}, {type(err)}')
        j = f'{err}'
    return j


@app.route('/<action>/reduced_axes', methods=['POST'])
def reduced_axes(action: str) -> Union[Response, str]:
    try:
        axes_lengths: Tuple[tuple, ...] = request.json['axes_lengths']
        axes_lengths = tuple(tuple(x) for x in axes_lengths)
        recipe = _prepare_transformation_recipe(request.json['eqn'],
                                                'max' if action == 'reduce' else action,
                                                axes_lengths)
        return jsonify(_reconstruct_from_shape(recipe,
                                               ims.shape)[reconstruct_fields['reduced_axes']])
    except EinopsError as err:
        print(f'unexpected {err}, {type(err)}')
        j = f'{err}'
    return j


@app.route('/<action>/axes_reordering', methods=['POST'])
def axes_reordering(action: str) -> Union[Response, str]:
    try:
        axes_lengths: Tuple[tuple, ...] = request.json['axes_lengths']
        axes_lengths = tuple(tuple(x) for x in axes_lengths)
        recipe = _prepare_transformation_recipe(request.json['eqn'],
                                                'max' if action == 'reduce' else action,
                                                axes_lengths)
        return jsonify(_reconstruct_from_shape(recipe,
                                               ims.shape)[reconstruct_fields['axes_reordering']])
    except EinopsError as err:
        print(f'unexpected {err}, {type(err)}')
        j = f'{err}'
    return j


@app.route('/<action>/added_axes_reconstruct', methods=['POST'])
def added_axes_reconstruct(action: str) -> Union[Response, str]:
    try:
        axes_lengths: Tuple[tuple, ...] = request.json['axes_lengths']
        axes_lengths = tuple(tuple(x) for x in axes_lengths)
        recipe = _prepare_transformation_recipe(request.json['eqn'],
                                                'max' if action == 'reduce' else action,
                                                axes_lengths)
        return jsonify(_reconstruct_from_shape(recipe,
                                               ims.shape)[reconstruct_fields['added_axes_reconstruct']])
    except EinopsError as err:
        print(f'unexpected {err}, {type(err)}')
        j = f'{err}'
    return j


@app.route('/<action>/final_shapes', methods=['POST'])
def final_shapes(action: str) -> Union[Response, str]:
    try:
        axes_lengths: Tuple[tuple, ...] = request.json['axes_lengths']
        axes_lengths = tuple(tuple(x) for x in axes_lengths)
        recipe = _prepare_transformation_recipe(request.json['eqn'],
                                                'max' if action == 'reduce' else action,
                                                axes_lengths)
        return jsonify(_reconstruct_from_shape(recipe,
                                               ims.shape)[reconstruct_fields['final_shapes']])
    except EinopsError as err:
        print(f'unexpected {err}, {type(err)}')
        j = f'{err}'
    return j

# _reconstruct_from_shape END
