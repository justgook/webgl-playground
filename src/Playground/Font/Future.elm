module Playground.Future exposing (font)


font =
    --http://wdobbie.com/post/gpu-text-rendering-with-vector-textures/
    --https://blog.mapbox.com/drawing-text-with-signed-distance-fields-in-mapbox-gl-b0933af6f817
    --https://github.com/Chlumsky/msdfgen
    ""


fragMSDF =
    --https://css-tricks.com/techniques-for-rendering-text-with-webgl/
    --Generate `npm install msdf-bmfont-xml -g`
    [glsl|

  uniform vec3 color;
  uniform sampler2D map;
  varying vec2 vUv;

  float median(float r, float g, float b) {
    return max(min(r, g), min(max(r, g), b));
  }

  void main(){
    vec4 texColor = texture2D(map, vUv);
    // Only render the inside of the glyph.
    float sigDist = median(texColor.r, texColor.g, texColor.b) - 0.5;
    float alpha = step(0.5, sigDist);
    gl_FragColor = vec4(color, alpha);
    if (gl_FragColor.a < 0.0001) discard;
  }
|]


fragSDF =
    [glsl|
  uniform vec3 color;
  uniform sampler2D map;
  varying vec2 vUv;

  void main(){
    vec4 texColor = texture2D(map, vUv);
    // Only render the inside of the glyph.
    float alpha = step(0.5, texColor.a);

    gl_FragColor = vec4(color, alpha);
    if (gl_FragColor.a < 0.0001) discard;
  }

|]
