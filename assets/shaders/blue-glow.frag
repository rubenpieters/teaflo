precision mediump float;

uniform float     time;
uniform sampler2D uSampler;
varying vec2      vTextureCoord;

const float blur = 1.0 / 512.0;

vec4 blueify(vec4 tex, float mult) {
	vec4 texColor;

  texColor = vec4(0.12 * tex.a, 0.3 * tex.a, tex.a, tex.a) * mult;

	return texColor;
}

void main( void ) {
  vec4 sum = vec4(0.0);
  vec4 sample = texture2D(uSampler, vTextureCoord.xy);

  if (sample.a > 0.5)
  {
    sum += sample;
  } else
  {
    sum += blueify(texture2D(uSampler, vec2(vTextureCoord.x - 1.5*blur, vTextureCoord.y)), 0.2);
    sum += blueify(texture2D(uSampler, vec2(vTextureCoord.x - 1.0*blur, vTextureCoord.y)), 0.2);
    sum += blueify(texture2D(uSampler, vec2(vTextureCoord.x - 0.5*blur, vTextureCoord.y)), 0.2);
    sum += blueify(texture2D(uSampler, vec2(vTextureCoord.x - blur, vTextureCoord.y)), 0.2);
    sum += blueify(texture2D(uSampler, vec2(vTextureCoord.x + blur, vTextureCoord.y)), 0.2);
    sum += blueify(texture2D(uSampler, vec2(vTextureCoord.x + 0.5*blur, vTextureCoord.y)), 0.2);
    sum += blueify(texture2D(uSampler, vec2(vTextureCoord.x + 1.0*blur, vTextureCoord.y)), 0.2);
    sum += blueify(texture2D(uSampler, vec2(vTextureCoord.x + 1.5*blur, vTextureCoord.y)), 0.2);
  }

  gl_FragColor = sum;
}