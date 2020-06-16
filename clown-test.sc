
~afile_path = "AudioFiles";

~tine_samples = {|i| Buffer.read(s, ~afile_path +/+ "Tine" ++ (i+1).asString ++ ".wav")} ! 12; // Iteration: p152, Buffer.read: p25

(
SynthDef(\tine, {|out=0, bufnum|
	//var env, impls, res;
	// env = EnvGen.kr(Env([0.02, 0], [3]), doneAction: Done.freeSelf);
	// impls = Decay.ar(Impulse.ar(0) + ClipNoise.ar(0.001), decayTime: 1.0);
	// Out.ar(0, [impls, impls]);
	// res = env * Klank.ar(`[[note, note*1.01, note*1.5, note*2.01, note*3.1], nil, [0.1, 0.1, 0.5, 0.6, 0.5]], impls);
	var samp;
	samp = PlayBuf.ar(1, bufnum, doneAction: Done.freeSelf);
	Out.ar(out, [samp, samp]);
}).add;
)


a = Synth(\tine, [bufnum: ~tine_samples.at(4)]);