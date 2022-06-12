from flask import Flask, jsonify, request
from einops.einops import _prepare_transformation_recipe
from werkzeug.exceptions import HTTPException

app = Flask(__name__)


# inspired by https://flask.palletsprojects.com/en/2.1.x/errorhandling/
@app.errorhandler(HTTPException)
def handle_exception(e: HTTPException) -> int:
    """Return string instead of HTML for HTTP errors."""
    # start with the correct headers and status code from the error
    response = e.get_response()
    # replace the body with JSON
    response.data = e
    response.content_type = "text/plain"
    return response


@app.route('/axes_permutation', methods=['POST'])
def axesPermutation():
    j = ''
    try:
        return jsonify(_prepare_transformation_recipe(request.json['eqn'],
                       'rearrange', ()).axes_permutation)
    except Exception as err:
        print(f'unexpected {err}, {type(err)}')
        j = f'{err}'
    return j


@app.route('/added_axes', methods=['POST'])
def addedAxes():
    j = ''
    try:
        return jsonify(_prepare_transformation_recipe(request.json['eqn'],
                       'rearrange', ()).added_axes)
    except Exception as err:
        print(f'unexpected {err}, {type(err)}')
        j = f'{err}'
    return j


@app.route('/output_composite_axes', methods=['POST'])
def outputCompositeAxes():
    j = ''
    try:
        return jsonify(_prepare_transformation_recipe(request.json['eqn'],
                       'rearrange', ()).output_composite_axes)
    except Exception as err:
        print(f'unexpected {err}, {type(err)}')
        j = f'{err}'
    return j


@app.route('/ellipsis_position_in_lhs', methods=['POST'])
def ellipsisPositionInLhs():
    j = ''
    try:
        return jsonify(_prepare_transformation_recipe(request.json['eqn'],
                       'rearrange', ()).ellipsis_position_in_lhs)
    except Exception as err:
        print(f'unexpected {err}, {type(err)}')
        j = f'{err}'
    return j


@app.route('/elementary_axes_lengths', methods=['POST'])
def elementaryAxesLengths():
    j = ''
    try:
        return jsonify(_prepare_transformation_recipe(request.json['eqn'],
                       'rearrange', ()).elementary_axes_lengths)
    except Exception as err:
        print(f'unexpected {err}, {type(err)}')
        j = f'{err}'
    return j


@app.route('/input_composite_axes', methods=['POST'])
def inputCompositeAxes():
    j = ''
    try:
        return jsonify(_prepare_transformation_recipe(request.json['eqn'],
                       'rearrange', ()).input_composite_axes)
    except Exception as err:
        print(f'unexpected {err}, {type(err)}')
        j = f'{err}'
    return j


@app.route('/reduced_elementary_axes', methods=['POST'])
def reducedElementaryAxes():
    j = ''
    try:
        return jsonify(_prepare_transformation_recipe(request.json['eqn'],
                       'rearrange', ()).reduced_elementary_axes)
    except Exception as err:
        print(f'unexpected {err}, {type(err)}')
        j = f'{err}'
    return j
