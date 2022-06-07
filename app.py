from Flask import Flask, jsonify, request
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


@app.route('/', methods=['POST'])
def maximumMay():
    # return jsonify(max(request.json['seq']))
    # return jsonify(_prepare_transformation_recipe(request.json['eqn'],
    # 'rearrange', ()).elementary_axes_lengths)
    # return jsonify(_prepare_transformation_recipe(request.json['eqn'],
    # 'rearrange', ()).input_composite_axes)
    # return jsonify(_prepare_transformation_recipe(request.json['eqn'],
    # 'rearrange', ()).reduced_elementary_axes)
    j = ''
    try:
        return jsonify(_prepare_transformation_recipe(request.json['eqn'],
                       'rearrange', ()).axes_permutation)
    except Exception as err:
        print(f'unexpected {err}, {type(err)}')
        j = f'{err}'
    return j

    # return jsonify(_prepare_transformation_recipe(request.json['eqn'],
    # 'rearrange', ()).added_axes)
    # return jsonify(_prepare_transformation_recipe(request.json['eqn'],
    # 'rearrange', ()).output_composite_axes)
    # return jsonify(_prepare_transformation_recipe(request.json['eqn'],
    # 'rearrange', ()).ellipsis_position_in_lhs)
