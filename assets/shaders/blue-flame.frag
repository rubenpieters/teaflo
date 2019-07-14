precision mediump float;

uniform float     time;
uniform sampler2D uSampler;
varying vec2      vTextureCoord;

const float blur = 1.0 / 512.0;
const int OCTAVES = 6;

float random(vec2 st) {
    return fract(sin(dot(st.xy, vec2(12.9898,78.233))) * 43758.5453123);
}

float noise(vec2 coord){
	vec2 i = floor(coord);
	vec2 f = fract(coord);

	// 4 corners of a rectangle surrounding our point
	float a = random(i);
	float b = random(i + vec2(1.0, 0.0));
	float c = random(i + vec2(0.0, 1.0));
	float d = random(i + vec2(1.0, 1.0));

	vec2 cubic = f * f * (3.0 - 2.0 * f);

	return mix(a, b, cubic.x) + (c - a) * cubic.y * (1.0 - cubic.x) + (d - b) * cubic.x * cubic.y;
}

float fbm(vec2 coord){
	float value = 0.0;
	float scale = 0.5;

	for(int i = 0; i < OCTAVES; i++){
		value += noise(coord) * scale;
		coord *= 2.0;
		scale *= 0.5;
	}
	return value;
}

vec4 blueify(vec4 tex, float v) {
	vec4 texColor;

  if (tex.a > 0.0)
  {
		float noise1 = noise(vTextureCoord * 75.0 + vec2(time * 0.25, -1.0 * time * 5.0));
		float noise2 = noise(vTextureCoord * 75.0 + vec2(time * 0.5, -1.0 * time * 7.0));
		float combined_noise = (noise1 + noise2) / 2.0 - v;
    texColor = vec4(0.0,0.0,combined_noise - (v / 10.0),tex.a);
  }
  else
  {
    texColor = tex;
  }

	return texColor;
}

void main( void ) {
  vec4 sum = vec4(0.0);
  vec4 tex = texture2D(uSampler, vTextureCoord.xy);
  vec4 texColor;

  sum += blueify(texture2D(uSampler, vec2(vTextureCoord.x - 4.0*blur, vTextureCoord.y)) * 0.005, 0.5);
  sum += blueify(texture2D(uSampler, vec2(vTextureCoord.x - 3.0*blur, vTextureCoord.y)) * 0.005, 0.4);
  sum += blueify(texture2D(uSampler, vec2(vTextureCoord.x - 2.0*blur, vTextureCoord.y)) * 0.005, 0.35);
  sum += blueify(texture2D(uSampler, vec2(vTextureCoord.x - blur, vTextureCoord.y)) * 0.005, 0.3);
  sum += texture2D(uSampler, vec2(vTextureCoord.x, vTextureCoord.y)) * 0.95;
  sum += blueify(texture2D(uSampler, vec2(vTextureCoord.x + blur, vTextureCoord.y)) * 0.005, 0.3);
  sum += blueify(texture2D(uSampler, vec2(vTextureCoord.x + 2.0*blur, vTextureCoord.y)) * 0.005, 0.35);
  sum += blueify(texture2D(uSampler, vec2(vTextureCoord.x + 3.0*blur, vTextureCoord.y)) * 0.005, 0.4);
  sum += blueify(texture2D(uSampler, vec2(vTextureCoord.x + 4.0*blur, vTextureCoord.y)) * 0.005, 0.5);

  gl_FragColor = sum;
}