%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2019. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%

%% OPENGL API

%% This file is generated DO NOT EDIT

%% @doc  Standard OpenGL api.
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">www.khronos.org</a>
%%
%% Booleans are represented by integers 0 and 1.

-module(gl).

-compile([{inline, get_interface/0}]).
-type enum() :: non_neg_integer().   %% See wx/include/gl.hrl
-type clamp() :: float().    %% 0.0..1.0
-type offset() :: non_neg_integer(). %% Offset in memory block
-type matrix12() :: {float(),float(),float(),float(),
                   float(),float(),float(),float(),
                   float(),float(),float(),float()}.
-type matrix16() :: {float(),float(),float(),float(),
                   float(),float(),float(),float(),
                   float(),float(),float(),float(),
                   float(),float(),float(),float()}.
-type matrix() :: matrix12() | matrix16().
-type mem() :: binary() | tuple().   %% Memory block
-on_load(init_nif/0).

-export([clearIndex/1,clearColor/4,clear/1,indexMask/1,colorMask/4,alphaFunc/2,
  blendFunc/2,logicOp/1,cullFace/1,frontFace/1,pointSize/1,lineWidth/1,
  lineStipple/2,polygonMode/2,polygonOffset/2,polygonStipple/1,getPolygonStipple/0,
  edgeFlag/1,edgeFlagv/1,scissor/4,clipPlane/2,getClipPlane/1,drawBuffer/1,
  readBuffer/1,enable/1,disable/1,isEnabled/1,enableClientState/1,disableClientState/1,
  getBooleanv/1,getDoublev/1,getFloatv/1,getIntegerv/1,pushAttrib/1,
  popAttrib/0,pushClientAttrib/1,popClientAttrib/0,renderMode/1,getError/0,
  getString/1,finish/0,flush/0,hint/2,clearDepth/1,depthFunc/1,depthMask/1,
  depthRange/2,clearAccum/4,accum/2,matrixMode/1,ortho/6,frustum/6,viewport/4,
  pushMatrix/0,popMatrix/0,loadIdentity/0,loadMatrixd/1,loadMatrixf/1,
  multMatrixd/1,multMatrixf/1,rotated/4,rotatef/4,scaled/3,scalef/3,translated/3,
  translatef/3,isList/1,deleteLists/2,genLists/1,newList/2,endList/0,
  callList/1,callLists/1,listBase/1,'begin'/1,'end'/0,vertex2d/2,vertex2f/2,
  vertex2i/2,vertex2s/2,vertex3d/3,vertex3f/3,vertex3i/3,vertex3s/3,vertex4d/4,
  vertex4f/4,vertex4i/4,vertex4s/4,vertex2dv/1,vertex2fv/1,vertex2iv/1,
  vertex2sv/1,vertex3dv/1,vertex3fv/1,vertex3iv/1,vertex3sv/1,vertex4dv/1,
  vertex4fv/1,vertex4iv/1,vertex4sv/1,normal3b/3,normal3d/3,normal3f/3,
  normal3i/3,normal3s/3,normal3bv/1,normal3dv/1,normal3fv/1,normal3iv/1,
  normal3sv/1,indexd/1,indexf/1,indexi/1,indexs/1,indexub/1,indexdv/1,
  indexfv/1,indexiv/1,indexsv/1,indexubv/1,color3b/3,color3d/3,color3f/3,
  color3i/3,color3s/3,color3ub/3,color3ui/3,color3us/3,color4b/4,color4d/4,
  color4f/4,color4i/4,color4s/4,color4ub/4,color4ui/4,color4us/4,color3bv/1,
  color3dv/1,color3fv/1,color3iv/1,color3sv/1,color3ubv/1,color3uiv/1,
  color3usv/1,color4bv/1,color4dv/1,color4fv/1,color4iv/1,color4sv/1,
  color4ubv/1,color4uiv/1,color4usv/1,texCoord1d/1,texCoord1f/1,texCoord1i/1,
  texCoord1s/1,texCoord2d/2,texCoord2f/2,texCoord2i/2,texCoord2s/2,texCoord3d/3,
  texCoord3f/3,texCoord3i/3,texCoord3s/3,texCoord4d/4,texCoord4f/4,texCoord4i/4,
  texCoord4s/4,texCoord1dv/1,texCoord1fv/1,texCoord1iv/1,texCoord1sv/1,
  texCoord2dv/1,texCoord2fv/1,texCoord2iv/1,texCoord2sv/1,texCoord3dv/1,
  texCoord3fv/1,texCoord3iv/1,texCoord3sv/1,texCoord4dv/1,texCoord4fv/1,
  texCoord4iv/1,texCoord4sv/1,rasterPos2d/2,rasterPos2f/2,rasterPos2i/2,
  rasterPos2s/2,rasterPos3d/3,rasterPos3f/3,rasterPos3i/3,rasterPos3s/3,
  rasterPos4d/4,rasterPos4f/4,rasterPos4i/4,rasterPos4s/4,rasterPos2dv/1,
  rasterPos2fv/1,rasterPos2iv/1,rasterPos2sv/1,rasterPos3dv/1,rasterPos3fv/1,
  rasterPos3iv/1,rasterPos3sv/1,rasterPos4dv/1,rasterPos4fv/1,rasterPos4iv/1,
  rasterPos4sv/1,rectd/4,rectf/4,recti/4,rects/4,rectdv/2,rectfv/2,rectiv/2,
  rectsv/2,vertexPointer/4,normalPointer/3,colorPointer/4,indexPointer/3,
  texCoordPointer/4,edgeFlagPointer/2,arrayElement/1,drawArrays/3,drawElements/4,
  interleavedArrays/3,shadeModel/1,lightf/3,lighti/3,lightfv/3,lightiv/3,
  getLightfv/2,getLightiv/2,lightModelf/2,lightModeli/2,lightModelfv/2,
  lightModeliv/2,materialf/3,materiali/3,materialfv/3,materialiv/3,getMaterialfv/2,
  getMaterialiv/2,colorMaterial/2,pixelZoom/2,pixelStoref/2,pixelStorei/2,
  pixelTransferf/2,pixelTransferi/2,pixelMapfv/3,pixelMapuiv/3,pixelMapusv/3,
  getPixelMapfv/2,getPixelMapuiv/2,getPixelMapusv/2,bitmap/7,readPixels/7,
  drawPixels/5,copyPixels/5,stencilFunc/3,stencilMask/1,stencilOp/3,
  clearStencil/1,texGend/3,texGenf/3,texGeni/3,texGendv/3,texGenfv/3,
  texGeniv/3,getTexGendv/2,getTexGenfv/2,getTexGeniv/2,texEnvf/3,texEnvi/3,
  texEnvfv/3,texEnviv/3,getTexEnvfv/2,getTexEnviv/2,texParameterf/3,
  texParameteri/3,texParameterfv/3,texParameteriv/3,getTexParameterfv/2,
  getTexParameteriv/2,getTexLevelParameterfv/3,getTexLevelParameteriv/3,
  texImage1D/8,texImage2D/9,getTexImage/5,genTextures/1,deleteTextures/1,
  bindTexture/2,prioritizeTextures/2,areTexturesResident/1,isTexture/1,
  texSubImage1D/7,texSubImage2D/9,copyTexImage1D/7,copyTexImage2D/8,
  copyTexSubImage1D/6,copyTexSubImage2D/8,map1d/6,map1f/6,map2d/10,map2f/10,
  getMapdv/3,getMapfv/3,getMapiv/3,evalCoord1d/1,evalCoord1f/1,evalCoord1dv/1,
  evalCoord1fv/1,evalCoord2d/2,evalCoord2f/2,evalCoord2dv/1,evalCoord2fv/1,
  mapGrid1d/3,mapGrid1f/3,mapGrid2d/6,mapGrid2f/6,evalPoint1/1,evalPoint2/2,
  evalMesh1/3,evalMesh2/5,fogf/2,fogi/2,fogfv/2,fogiv/2,feedbackBuffer/3,
  passThrough/1,selectBuffer/2,initNames/0,loadName/1,pushName/1,popName/0,
  blendColor/4,blendEquation/1,drawRangeElements/6,texImage3D/10,texSubImage3D/11,
  copyTexSubImage3D/9,colorTable/6,colorTableParameterfv/3,colorTableParameteriv/3,
  copyColorTable/5,getColorTable/4,getColorTableParameterfv/2,getColorTableParameteriv/2,
  colorSubTable/6,copyColorSubTable/5,convolutionFilter1D/6,convolutionFilter2D/7,
  convolutionParameterf/3,convolutionParameterfv/3,convolutionParameteri/3,
  convolutionParameteriv/3,copyConvolutionFilter1D/5,copyConvolutionFilter2D/6,
  getConvolutionFilter/4,getConvolutionParameterfv/2,getConvolutionParameteriv/2,
  separableFilter2D/8,getHistogram/5,getHistogramParameterfv/2,getHistogramParameteriv/2,
  getMinmax/5,getMinmaxParameterfv/2,getMinmaxParameteriv/2,histogram/4,
  minmax/3,resetHistogram/1,resetMinmax/1,activeTexture/1,sampleCoverage/2,
  compressedTexImage3D/9,compressedTexImage2D/8,compressedTexImage1D/7,
  compressedTexSubImage3D/11,compressedTexSubImage2D/9,compressedTexSubImage1D/7,
  getCompressedTexImage/3,clientActiveTexture/1,multiTexCoord1d/2,
  multiTexCoord1dv/2,multiTexCoord1f/2,multiTexCoord1fv/2,multiTexCoord1i/2,
  multiTexCoord1iv/2,multiTexCoord1s/2,multiTexCoord1sv/2,multiTexCoord2d/3,
  multiTexCoord2dv/2,multiTexCoord2f/3,multiTexCoord2fv/2,multiTexCoord2i/3,
  multiTexCoord2iv/2,multiTexCoord2s/3,multiTexCoord2sv/2,multiTexCoord3d/4,
  multiTexCoord3dv/2,multiTexCoord3f/4,multiTexCoord3fv/2,multiTexCoord3i/4,
  multiTexCoord3iv/2,multiTexCoord3s/4,multiTexCoord3sv/2,multiTexCoord4d/5,
  multiTexCoord4dv/2,multiTexCoord4f/5,multiTexCoord4fv/2,multiTexCoord4i/5,
  multiTexCoord4iv/2,multiTexCoord4s/5,multiTexCoord4sv/2,loadTransposeMatrixf/1,
  loadTransposeMatrixd/1,multTransposeMatrixf/1,multTransposeMatrixd/1,
  blendFuncSeparate/4,multiDrawArrays/3,pointParameterf/2,pointParameterfv/2,
  pointParameteri/2,pointParameteriv/2,fogCoordf/1,fogCoordfv/1,fogCoordd/1,
  fogCoorddv/1,fogCoordPointer/3,secondaryColor3b/3,secondaryColor3bv/1,
  secondaryColor3d/3,secondaryColor3dv/1,secondaryColor3f/3,secondaryColor3fv/1,
  secondaryColor3i/3,secondaryColor3iv/1,secondaryColor3s/3,secondaryColor3sv/1,
  secondaryColor3ub/3,secondaryColor3ubv/1,secondaryColor3ui/3,secondaryColor3uiv/1,
  secondaryColor3us/3,secondaryColor3usv/1,secondaryColorPointer/4,
  windowPos2d/2,windowPos2dv/1,windowPos2f/2,windowPos2fv/1,windowPos2i/2,
  windowPos2iv/1,windowPos2s/2,windowPos2sv/1,windowPos3d/3,windowPos3dv/1,
  windowPos3f/3,windowPos3fv/1,windowPos3i/3,windowPos3iv/1,windowPos3s/3,
  windowPos3sv/1,genQueries/1,deleteQueries/1,isQuery/1,beginQuery/2,
  endQuery/1,getQueryiv/2,getQueryObjectiv/2,getQueryObjectuiv/2,bindBuffer/2,
  deleteBuffers/1,genBuffers/1,isBuffer/1,bufferData/4,bufferSubData/4,
  getBufferSubData/4,getBufferParameteriv/2,blendEquationSeparate/2,
  drawBuffers/1,stencilOpSeparate/4,stencilFuncSeparate/4,stencilMaskSeparate/2,
  attachShader/2,bindAttribLocation/3,compileShader/1,createProgram/0,
  createShader/1,deleteProgram/1,deleteShader/1,detachShader/2,disableVertexAttribArray/1,
  enableVertexAttribArray/1,getActiveAttrib/3,getActiveUniform/3,getAttachedShaders/2,
  getAttribLocation/2,getProgramiv/2,getProgramInfoLog/2,getShaderiv/2,
  getShaderInfoLog/2,getShaderSource/2,getUniformLocation/2,getUniformfv/2,
  getUniformiv/2,getVertexAttribdv/2,getVertexAttribfv/2,getVertexAttribiv/2,
  isProgram/1,isShader/1,linkProgram/1,shaderSource/2,useProgram/1,uniform1f/2,
  uniform2f/3,uniform3f/4,uniform4f/5,uniform1i/2,uniform2i/3,uniform3i/4,
  uniform4i/5,uniform1fv/2,uniform2fv/2,uniform3fv/2,uniform4fv/2,uniform1iv/2,
  uniform2iv/2,uniform3iv/2,uniform4iv/2,uniformMatrix2fv/3,uniformMatrix3fv/3,
  uniformMatrix4fv/3,validateProgram/1,vertexAttrib1d/2,vertexAttrib1dv/2,
  vertexAttrib1f/2,vertexAttrib1fv/2,vertexAttrib1s/2,vertexAttrib1sv/2,
  vertexAttrib2d/3,vertexAttrib2dv/2,vertexAttrib2f/3,vertexAttrib2fv/2,
  vertexAttrib2s/3,vertexAttrib2sv/2,vertexAttrib3d/4,vertexAttrib3dv/2,
  vertexAttrib3f/4,vertexAttrib3fv/2,vertexAttrib3s/4,vertexAttrib3sv/2,
  vertexAttrib4Nbv/2,vertexAttrib4Niv/2,vertexAttrib4Nsv/2,vertexAttrib4Nub/5,
  vertexAttrib4Nubv/2,vertexAttrib4Nuiv/2,vertexAttrib4Nusv/2,vertexAttrib4bv/2,
  vertexAttrib4d/5,vertexAttrib4dv/2,vertexAttrib4f/5,vertexAttrib4fv/2,
  vertexAttrib4iv/2,vertexAttrib4s/5,vertexAttrib4sv/2,vertexAttrib4ubv/2,
  vertexAttrib4uiv/2,vertexAttrib4usv/2,vertexAttribPointer/6,uniformMatrix2x3fv/3,
  uniformMatrix3x2fv/3,uniformMatrix2x4fv/3,uniformMatrix4x2fv/3,uniformMatrix3x4fv/3,
  uniformMatrix4x3fv/3,colorMaski/5,getBooleani_v/2,getIntegeri_v/2,
  enablei/2,disablei/2,isEnabledi/2,beginTransformFeedback/1,endTransformFeedback/0,
  bindBufferRange/5,bindBufferBase/3,transformFeedbackVaryings/3,getTransformFeedbackVarying/3,
  clampColor/2,beginConditionalRender/2,endConditionalRender/0,vertexAttribIPointer/5,
  getVertexAttribIiv/2,getVertexAttribIuiv/2,vertexAttribI1i/2,vertexAttribI2i/3,
  vertexAttribI3i/4,vertexAttribI4i/5,vertexAttribI1ui/2,vertexAttribI2ui/3,
  vertexAttribI3ui/4,vertexAttribI4ui/5,vertexAttribI1iv/2,vertexAttribI2iv/2,
  vertexAttribI3iv/2,vertexAttribI4iv/2,vertexAttribI1uiv/2,vertexAttribI2uiv/2,
  vertexAttribI3uiv/2,vertexAttribI4uiv/2,vertexAttribI4bv/2,vertexAttribI4sv/2,
  vertexAttribI4ubv/2,vertexAttribI4usv/2,getUniformuiv/2,bindFragDataLocation/3,
  getFragDataLocation/2,uniform1ui/2,uniform2ui/3,uniform3ui/4,uniform4ui/5,
  uniform1uiv/2,uniform2uiv/2,uniform3uiv/2,uniform4uiv/2,texParameterIiv/3,
  texParameterIuiv/3,getTexParameterIiv/2,getTexParameterIuiv/2,clearBufferiv/3,
  clearBufferuiv/3,clearBufferfv/3,clearBufferfi/4,getStringi/2,drawArraysInstanced/4,
  drawElementsInstanced/5,texBuffer/3,primitiveRestartIndex/1,getInteger64i_v/2,
  getBufferParameteri64v/2,framebufferTexture/4,vertexAttribDivisor/2,
  minSampleShading/1,blendEquationi/2,blendEquationSeparatei/3,blendFunci/3,
  blendFuncSeparatei/5,loadTransposeMatrixfARB/1,loadTransposeMatrixdARB/1,
  multTransposeMatrixfARB/1,multTransposeMatrixdARB/1,weightbvARB/1,
  weightsvARB/1,weightivARB/1,weightfvARB/1,weightdvARB/1,weightubvARB/1,
  weightusvARB/1,weightuivARB/1,vertexBlendARB/1,currentPaletteMatrixARB/1,
  matrixIndexubvARB/1,matrixIndexusvARB/1,matrixIndexuivARB/1,programStringARB/3,
  bindProgramARB/2,deleteProgramsARB/1,genProgramsARB/1,programEnvParameter4dARB/6,
  programEnvParameter4dvARB/3,programEnvParameter4fARB/6,programEnvParameter4fvARB/3,
  programLocalParameter4dARB/6,programLocalParameter4dvARB/3,programLocalParameter4fARB/6,
  programLocalParameter4fvARB/3,getProgramEnvParameterdvARB/2,getProgramEnvParameterfvARB/2,
  getProgramLocalParameterdvARB/2,getProgramLocalParameterfvARB/2,
  getProgramStringARB/3,getBufferParameterivARB/2,deleteObjectARB/1,
  getHandleARB/1,detachObjectARB/2,createShaderObjectARB/1,shaderSourceARB/2,
  compileShaderARB/1,createProgramObjectARB/0,attachObjectARB/2,linkProgramARB/1,
  useProgramObjectARB/1,validateProgramARB/1,getObjectParameterfvARB/2,
  getObjectParameterivARB/2,getInfoLogARB/2,getAttachedObjectsARB/2,
  getUniformLocationARB/2,getActiveUniformARB/3,getUniformfvARB/2,
  getUniformivARB/2,getShaderSourceARB/2,bindAttribLocationARB/3,getActiveAttribARB/3,
  getAttribLocationARB/2,isRenderbuffer/1,bindRenderbuffer/2,deleteRenderbuffers/1,
  genRenderbuffers/1,renderbufferStorage/4,getRenderbufferParameteriv/2,
  isFramebuffer/1,bindFramebuffer/2,deleteFramebuffers/1,genFramebuffers/1,
  checkFramebufferStatus/1,framebufferTexture1D/5,framebufferTexture2D/5,
  framebufferTexture3D/6,framebufferRenderbuffer/4,getFramebufferAttachmentParameteriv/3,
  generateMipmap/1,blitFramebuffer/10,renderbufferStorageMultisample/5,
  framebufferTextureLayer/5,framebufferTextureFaceARB/5,flushMappedBufferRange/3,
  bindVertexArray/1,deleteVertexArrays/1,genVertexArrays/1,isVertexArray/1,
  getUniformIndices/2,getActiveUniformsiv/3,getActiveUniformName/3,
  getUniformBlockIndex/2,getActiveUniformBlockiv/4,getActiveUniformBlockName/3,
  uniformBlockBinding/3,copyBufferSubData/5,drawElementsBaseVertex/5,
  drawRangeElementsBaseVertex/7,drawElementsInstancedBaseVertex/6,
  provokingVertex/1,fenceSync/2,isSync/1,deleteSync/1,clientWaitSync/3,
  waitSync/3,getInteger64v/1,getSynciv/3,texImage2DMultisample/6,texImage3DMultisample/7,
  getMultisamplefv/2,sampleMaski/2,namedStringARB/3,deleteNamedStringARB/1,
  compileShaderIncludeARB/2,isNamedStringARB/1,getNamedStringARB/2,
  getNamedStringivARB/2,bindFragDataLocationIndexed/4,getFragDataIndex/2,
  genSamplers/1,deleteSamplers/1,isSampler/1,bindSampler/2,samplerParameteri/3,
  samplerParameteriv/3,samplerParameterf/3,samplerParameterfv/3,samplerParameterIiv/3,
  samplerParameterIuiv/3,getSamplerParameteriv/2,getSamplerParameterIiv/2,
  getSamplerParameterfv/2,getSamplerParameterIuiv/2,queryCounter/2,
  getQueryObjecti64v/2,getQueryObjectui64v/2,drawArraysIndirect/2,
  drawElementsIndirect/3,uniform1d/2,uniform2d/3,uniform3d/4,uniform4d/5,
  uniform1dv/2,uniform2dv/2,uniform3dv/2,uniform4dv/2,uniformMatrix2dv/3,
  uniformMatrix3dv/3,uniformMatrix4dv/3,uniformMatrix2x3dv/3,uniformMatrix2x4dv/3,
  uniformMatrix3x2dv/3,uniformMatrix3x4dv/3,uniformMatrix4x2dv/3,uniformMatrix4x3dv/3,
  getUniformdv/2,getSubroutineUniformLocation/3,getSubroutineIndex/3,
  getActiveSubroutineUniformName/4,getActiveSubroutineName/4,uniformSubroutinesuiv/2,
  getUniformSubroutineuiv/2,getProgramStageiv/3,patchParameteri/2,
  patchParameterfv/2,bindTransformFeedback/2,deleteTransformFeedbacks/1,
  genTransformFeedbacks/1,isTransformFeedback/1,pauseTransformFeedback/0,
  resumeTransformFeedback/0,drawTransformFeedback/2,drawTransformFeedbackStream/3,
  beginQueryIndexed/3,endQueryIndexed/2,getQueryIndexediv/3,releaseShaderCompiler/0,
  shaderBinary/3,getShaderPrecisionFormat/2,depthRangef/2,clearDepthf/1,
  getProgramBinary/2,programBinary/3,programParameteri/3,useProgramStages/3,
  activeShaderProgram/2,createShaderProgramv/2,bindProgramPipeline/1,
  deleteProgramPipelines/1,genProgramPipelines/1,isProgramPipeline/1,
  getProgramPipelineiv/2,programUniform1i/3,programUniform1iv/3,programUniform1f/3,
  programUniform1fv/3,programUniform1d/3,programUniform1dv/3,programUniform1ui/3,
  programUniform1uiv/3,programUniform2i/4,programUniform2iv/3,programUniform2f/4,
  programUniform2fv/3,programUniform2d/4,programUniform2dv/3,programUniform2ui/4,
  programUniform2uiv/3,programUniform3i/5,programUniform3iv/3,programUniform3f/5,
  programUniform3fv/3,programUniform3d/5,programUniform3dv/3,programUniform3ui/5,
  programUniform3uiv/3,programUniform4i/6,programUniform4iv/3,programUniform4f/6,
  programUniform4fv/3,programUniform4d/6,programUniform4dv/3,programUniform4ui/6,
  programUniform4uiv/3,programUniformMatrix2fv/4,programUniformMatrix3fv/4,
  programUniformMatrix4fv/4,programUniformMatrix2dv/4,programUniformMatrix3dv/4,
  programUniformMatrix4dv/4,programUniformMatrix2x3fv/4,programUniformMatrix3x2fv/4,
  programUniformMatrix2x4fv/4,programUniformMatrix4x2fv/4,programUniformMatrix3x4fv/4,
  programUniformMatrix4x3fv/4,programUniformMatrix2x3dv/4,programUniformMatrix3x2dv/4,
  programUniformMatrix2x4dv/4,programUniformMatrix4x2dv/4,programUniformMatrix3x4dv/4,
  programUniformMatrix4x3dv/4,validateProgramPipeline/1,getProgramPipelineInfoLog/2,
  vertexAttribL1d/2,vertexAttribL2d/3,vertexAttribL3d/4,vertexAttribL4d/5,
  vertexAttribL1dv/2,vertexAttribL2dv/2,vertexAttribL3dv/2,vertexAttribL4dv/2,
  vertexAttribLPointer/5,getVertexAttribLdv/2,viewportArrayv/2,viewportIndexedf/5,
  viewportIndexedfv/2,scissorArrayv/2,scissorIndexed/5,scissorIndexedv/2,
  depthRangeArrayv/2,depthRangeIndexed/3,getFloati_v/2,getDoublei_v/2,
  debugMessageControlARB/5,debugMessageInsertARB/5,getDebugMessageLogARB/2,
  getGraphicsResetStatusARB/0,drawArraysInstancedBaseInstance/5,drawElementsInstancedBaseInstance/6,
  drawElementsInstancedBaseVertexBaseInstance/7,drawTransformFeedbackInstanced/3,
  drawTransformFeedbackStreamInstanced/4,getInternalformativ/4,bindImageTexture/7,
  memoryBarrier/1,texStorage1D/4,texStorage2D/5,texStorage3D/6,depthBoundsEXT/2,
  stencilClearTagEXT/2]).

-export([get_interface/0, rec/1, lookup_func/0]).
-define(nif_stub,nif_stub_error(?LINE)).
%% @hidden
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

%% @hidden
init_nif() ->
  Base = "erl_gl",
  Priv = code:priv_dir(wx),
  SrcTree = filename:join(Priv,erlang:system_info(system_architecture)),
  NifFile = case filelib:is_dir(SrcTree) of
                true -> filename:absname(filename:join(SrcTree, Base));
                false -> filename:absname(filename:join(Priv, Base))
            end,
  erlang:load_nif(NifFile, 0).

%% @hidden
get_interface() ->
    wxe_util.  %% temporary

%% @hidden
rec(Op) ->
    receive
        {'_egl_result_', Res} -> Res;
        {'_egl_error_',  Op, Res} -> error({error,Res,Op});
        {'_egl_error_', Other, Res} ->
             Err = io_lib:format("~p in op: ~p", [Res, Other]),
            error_logger:error_report([{gl, error}, {message, lists:flatten(Err)}]),
            rec(Op)
    end.

lookup_func() -> ?nif_stub.




%% API

%% @doc glClearIndex
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec clearIndex(C) -> 'ok' when C :: float().
clearIndex(C) when is_float(C) ->
  IF = get_interface(),
  IF:queue_cmd(C,5037),
  ok.

%% @doc glClearColor
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec clearColor(Red, Green, Blue, Alpha) -> 'ok' when Red :: clamp(),Green :: clamp(),Blue :: clamp(),Alpha :: clamp().
clearColor(Red,Green,Blue,Alpha) when is_float(Red),is_float(Green),is_float(Blue),is_float(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5038),
  ok.

%% @doc glClear
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec clear(Mask) -> 'ok' when Mask :: integer().
clear(Mask) when is_integer(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(Mask,5039),
  ok.

%% @doc glIndexMask
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec indexMask(Mask) -> 'ok' when Mask :: integer().
indexMask(Mask) when is_integer(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(Mask,5040),
  ok.

%% @doc glColorMask
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec colorMask(Red, Green, Blue, Alpha) -> 'ok' when Red :: 0|1,Green :: 0|1,Blue :: 0|1,Alpha :: 0|1.
colorMask(Red,Green,Blue,Alpha) when (0 =:= Red) orelse (1 =:= Red),(0 =:= Green) orelse (1 =:= Green),(0 =:= Blue) orelse (1 =:= Blue),(0 =:= Alpha) orelse (1 =:= Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5041),
  ok.

%% @doc glAlphaFunc
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec alphaFunc(Func, Ref) -> 'ok' when Func :: enum(),Ref :: clamp().
alphaFunc(Func,Ref) when is_integer(Func),is_float(Ref) ->
  IF = get_interface(),
  IF:queue_cmd(Func,Ref,5042),
  ok.

%% @doc glBlendFunc
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec blendFunc(Sfactor, Dfactor) -> 'ok' when Sfactor :: enum(),Dfactor :: enum().
blendFunc(Sfactor,Dfactor) when is_integer(Sfactor),is_integer(Dfactor) ->
  IF = get_interface(),
  IF:queue_cmd(Sfactor,Dfactor,5043),
  ok.

%% @doc glLogicOp
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec logicOp(Opcode) -> 'ok' when Opcode :: enum().
logicOp(Opcode) when is_integer(Opcode) ->
  IF = get_interface(),
  IF:queue_cmd(Opcode,5044),
  ok.

%% @doc glCullFace
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec cullFace(Mode) -> 'ok' when Mode :: enum().
cullFace(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5045),
  ok.

%% @doc glFrontFace
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec frontFace(Mode) -> 'ok' when Mode :: enum().
frontFace(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5046),
  ok.

%% @doc glPointSize
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec pointSize(Size) -> 'ok' when Size :: float().
pointSize(Size) when is_float(Size) ->
  IF = get_interface(),
  IF:queue_cmd(Size,5047),
  ok.

%% @doc glLineWidth
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec lineWidth(Width) -> 'ok' when Width :: float().
lineWidth(Width) when is_float(Width) ->
  IF = get_interface(),
  IF:queue_cmd(Width,5048),
  ok.

%% @doc glLineStipple
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec lineStipple(Factor, Pattern) -> 'ok' when Factor :: integer(),Pattern :: integer().
lineStipple(Factor,Pattern) when is_integer(Factor),is_integer(Pattern) ->
  IF = get_interface(),
  IF:queue_cmd(Factor,Pattern,5049),
  ok.

%% @doc glPolygonMode
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec polygonMode(Face, Mode) -> 'ok' when Face :: enum(),Mode :: enum().
polygonMode(Face,Mode) when is_integer(Face),is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Mode,5050),
  ok.

%% @doc glPolygonOffset
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec polygonOffset(Factor, Units) -> 'ok' when Factor :: float(),Units :: float().
polygonOffset(Factor,Units) when is_float(Factor),is_float(Units) ->
  IF = get_interface(),
  IF:queue_cmd(Factor,Units,5051),
  ok.

%% @doc glPolygonStipple
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec polygonStipple(Mask) -> 'ok' when Mask :: binary().
polygonStipple(Mask) when is_binary(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(Mask,5052),
  ok.

%% @doc glGetPolygonStipple
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getPolygonStipple() -> binary().
getPolygonStipple()  ->
  IF = get_interface(),
  IF:queue_cmd(5053),
  rec(5053).

%% @doc glEdgeFlag
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec edgeFlag(Flag) -> 'ok' when Flag :: 0|1.
edgeFlag(Flag) when (0 =:= Flag) orelse (1 =:= Flag) ->
  IF = get_interface(),
  IF:queue_cmd(Flag,5054),
  ok.

%% @equiv edgeFlag(Flag)
-spec edgeFlagv(Flag) -> 'ok' when Flag :: {Flag :: 0|1}.
edgeFlagv({Flag}) ->  edgeFlag(Flag).
%% @doc glScissor
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec scissor(X, Y, Width, Height) -> 'ok' when X :: integer(),Y :: integer(),Width :: integer(),Height :: integer().
scissor(X,Y,Width,Height) when is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Width,Height,5055),
  ok.

%% @doc glClipPlane
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec clipPlane(Plane, Equation) -> 'ok' when Plane :: enum(),Equation :: {float(),float(),float(),float()}.
clipPlane(Plane,Equation) when is_integer(Plane),tuple_size(Equation) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Plane,Equation,5056),
  ok.

%% @doc glGetClipPlane
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getClipPlane(Plane) -> {float(),float(),float(),float()} when Plane :: enum().
getClipPlane(Plane) when is_integer(Plane) ->
  IF = get_interface(),
  IF:queue_cmd(Plane,5057),
  rec(5057).

%% @doc glDrawBuffer
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec drawBuffer(Mode) -> 'ok' when Mode :: enum().
drawBuffer(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5058),
  ok.

%% @doc glReadBuffer
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec readBuffer(Mode) -> 'ok' when Mode :: enum().
readBuffer(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5059),
  ok.

%% @doc glEnable
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec enable(Cap) -> 'ok' when Cap :: enum().
enable(Cap) when is_integer(Cap) ->
  IF = get_interface(),
  IF:queue_cmd(Cap,5060),
  ok.

%% @doc glEnable
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec disable(Cap) -> 'ok' when Cap :: enum().
disable(Cap) when is_integer(Cap) ->
  IF = get_interface(),
  IF:queue_cmd(Cap,5061),
  ok.

%% @doc glIsEnabled
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec isEnabled(Cap) -> 0|1 when Cap :: enum().
isEnabled(Cap) when is_integer(Cap) ->
  IF = get_interface(),
  IF:queue_cmd(Cap,5062),
  rec(5062).

%% @doc glEnableClientState
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec enableClientState(Cap) -> 'ok' when Cap :: enum().
enableClientState(Cap) when is_integer(Cap) ->
  IF = get_interface(),
  IF:queue_cmd(Cap,5063),
  ok.

%% @doc glEnableClientState
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec disableClientState(Cap) -> 'ok' when Cap :: enum().
disableClientState(Cap) when is_integer(Cap) ->
  IF = get_interface(),
  IF:queue_cmd(Cap,5064),
  ok.

%% @doc glGet
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getBooleanv(Pname) -> [0|1] when Pname :: enum().
getBooleanv(Pname) when is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,5065),
  rec(5065).

%% @doc glGet
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getDoublev(Pname) -> [float()] when Pname :: enum().
getDoublev(Pname) when is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,5066),
  rec(5066).

%% @doc glGet
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getFloatv(Pname) -> [float()] when Pname :: enum().
getFloatv(Pname) when is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,5067),
  rec(5067).

%% @doc glGet
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getIntegerv(Pname) -> [integer()] when Pname :: enum().
getIntegerv(Pname) when is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,5068),
  rec(5068).

%% @doc glPushAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec pushAttrib(Mask) -> 'ok' when Mask :: integer().
pushAttrib(Mask) when is_integer(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(Mask,5069),
  ok.

%% @doc glPushAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec popAttrib() -> 'ok'.
popAttrib()  ->
  IF = get_interface(),
  IF:queue_cmd(5070),
  ok.

%% @doc glPushClientAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec pushClientAttrib(Mask) -> 'ok' when Mask :: integer().
pushClientAttrib(Mask) when is_integer(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(Mask,5071),
  ok.

%% @doc glPushClientAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec popClientAttrib() -> 'ok'.
popClientAttrib()  ->
  IF = get_interface(),
  IF:queue_cmd(5072),
  ok.

%% @doc glRenderMode
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec renderMode(Mode) -> integer() when Mode :: enum().
renderMode(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5073),
  rec(5073).

%% @doc glGetError
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getError() -> enum().
getError()  ->
  IF = get_interface(),
  IF:queue_cmd(5074),
  rec(5074).

%% @doc glGetString
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getString(Name) -> string() when Name :: enum().
getString(Name) when is_integer(Name) ->
  IF = get_interface(),
  IF:queue_cmd(Name,5075),
  rec(5075).

%% @doc glFinish
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec finish() -> 'ok'.
finish()  ->
  IF = get_interface(),
  IF:queue_cmd(5076),
  ok.

%% @doc glFlush
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec flush() -> 'ok'.
flush()  ->
  IF = get_interface(),
  IF:queue_cmd(5077),
  ok.

%% @doc glHint
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec hint(Target, Mode) -> 'ok' when Target :: enum(),Mode :: enum().
hint(Target,Mode) when is_integer(Target),is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Mode,5078),
  ok.

%% @doc glClearDepth
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec clearDepth(Depth) -> 'ok' when Depth :: clamp().
clearDepth(Depth) when is_float(Depth) ->
  IF = get_interface(),
  IF:queue_cmd(Depth,5079),
  ok.

%% @doc glDepthFunc
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec depthFunc(Func) -> 'ok' when Func :: enum().
depthFunc(Func) when is_integer(Func) ->
  IF = get_interface(),
  IF:queue_cmd(Func,5080),
  ok.

%% @doc glDepthMask
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec depthMask(Flag) -> 'ok' when Flag :: 0|1.
depthMask(Flag) when (0 =:= Flag) orelse (1 =:= Flag) ->
  IF = get_interface(),
  IF:queue_cmd(Flag,5081),
  ok.

%% @doc glDepthRange
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec depthRange(Near_val, Far_val) -> 'ok' when Near_val :: clamp(),Far_val :: clamp().
depthRange(Near_val,Far_val) when is_float(Near_val),is_float(Far_val) ->
  IF = get_interface(),
  IF:queue_cmd(Near_val,Far_val,5082),
  ok.

%% @doc glClearAccum
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec clearAccum(Red, Green, Blue, Alpha) -> 'ok' when Red :: float(),Green :: float(),Blue :: float(),Alpha :: float().
clearAccum(Red,Green,Blue,Alpha) when is_float(Red),is_float(Green),is_float(Blue),is_float(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5083),
  ok.

%% @doc glAccum
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec accum(Op, Value) -> 'ok' when Op :: enum(),Value :: float().
accum(Op,Value) when is_integer(Op),is_float(Value) ->
  IF = get_interface(),
  IF:queue_cmd(Op,Value,5084),
  ok.

%% @doc glMatrixMode
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec matrixMode(Mode) -> 'ok' when Mode :: enum().
matrixMode(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5085),
  ok.

%% @doc glOrtho
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec ortho(Left, Right, Bottom, Top, Near_val, Far_val) -> 'ok' when Left :: float(),Right :: float(),Bottom :: float(),Top :: float(),Near_val :: float(),Far_val :: float().
ortho(Left,Right,Bottom,Top,Near_val,Far_val) when is_float(Left),is_float(Right),is_float(Bottom),is_float(Top),is_float(Near_val),is_float(Far_val) ->
  IF = get_interface(),
  IF:queue_cmd(Left,Right,Bottom,Top,Near_val,Far_val,5086),
  ok.

%% @doc glFrustum
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec frustum(Left, Right, Bottom, Top, Near_val, Far_val) -> 'ok' when Left :: float(),Right :: float(),Bottom :: float(),Top :: float(),Near_val :: float(),Far_val :: float().
frustum(Left,Right,Bottom,Top,Near_val,Far_val) when is_float(Left),is_float(Right),is_float(Bottom),is_float(Top),is_float(Near_val),is_float(Far_val) ->
  IF = get_interface(),
  IF:queue_cmd(Left,Right,Bottom,Top,Near_val,Far_val,5087),
  ok.

%% @doc glViewport
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec viewport(X, Y, Width, Height) -> 'ok' when X :: integer(),Y :: integer(),Width :: integer(),Height :: integer().
viewport(X,Y,Width,Height) when is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Width,Height,5088),
  ok.

%% @doc glPushMatrix
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec pushMatrix() -> 'ok'.
pushMatrix()  ->
  IF = get_interface(),
  IF:queue_cmd(5089),
  ok.

%% @doc glPushMatrix
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec popMatrix() -> 'ok'.
popMatrix()  ->
  IF = get_interface(),
  IF:queue_cmd(5090),
  ok.

%% @doc glLoadIdentity
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec loadIdentity() -> 'ok'.
loadIdentity()  ->
  IF = get_interface(),
  IF:queue_cmd(5091),
  ok.

%% @doc glLoadMatrix
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec loadMatrixd(M) -> 'ok' when M :: matrix().
loadMatrixd(M) when tuple_size(M) =:= 16 ->
  IF = get_interface(),
  IF:queue_cmd(M,5092),
  ok.

%% @doc glLoadMatrix
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec loadMatrixf(M) -> 'ok' when M :: matrix().
loadMatrixf(M) when tuple_size(M) =:= 16 ->
  IF = get_interface(),
  IF:queue_cmd(M,5093),
  ok.

%% @doc glMultMatrix
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec multMatrixd(M) -> 'ok' when M :: matrix().
multMatrixd(M) when tuple_size(M) =:= 16 ->
  IF = get_interface(),
  IF:queue_cmd(M,5094),
  ok.

%% @doc glMultMatrix
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec multMatrixf(M) -> 'ok' when M :: matrix().
multMatrixf(M) when tuple_size(M) =:= 16 ->
  IF = get_interface(),
  IF:queue_cmd(M,5095),
  ok.

%% @doc glRotate
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec rotated(Angle, X, Y, Z) -> 'ok' when Angle :: float(),X :: float(),Y :: float(),Z :: float().
rotated(Angle,X,Y,Z) when is_float(Angle),is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Angle,X,Y,Z,5096),
  ok.

%% @doc glRotate
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec rotatef(Angle, X, Y, Z) -> 'ok' when Angle :: float(),X :: float(),Y :: float(),Z :: float().
rotatef(Angle,X,Y,Z) when is_float(Angle),is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Angle,X,Y,Z,5097),
  ok.

%% @doc glScale
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec scaled(X, Y, Z) -> 'ok' when X :: float(),Y :: float(),Z :: float().
scaled(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5098),
  ok.

%% @doc glScale
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec scalef(X, Y, Z) -> 'ok' when X :: float(),Y :: float(),Z :: float().
scalef(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5099),
  ok.

%% @doc glTranslate
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec translated(X, Y, Z) -> 'ok' when X :: float(),Y :: float(),Z :: float().
translated(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5100),
  ok.

%% @doc glTranslate
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec translatef(X, Y, Z) -> 'ok' when X :: float(),Y :: float(),Z :: float().
translatef(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5101),
  ok.

%% @doc glIsList
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec isList(List) -> 0|1 when List :: integer().
isList(List) when is_integer(List) ->
  IF = get_interface(),
  IF:queue_cmd(List,5102),
  rec(5102).

%% @doc glDeleteLists
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec deleteLists(List, Range) -> 'ok' when List :: integer(),Range :: integer().
deleteLists(List,Range) when is_integer(List),is_integer(Range) ->
  IF = get_interface(),
  IF:queue_cmd(List,Range,5103),
  ok.

%% @doc glGenLists
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec genLists(Range) -> integer() when Range :: integer().
genLists(Range) when is_integer(Range) ->
  IF = get_interface(),
  IF:queue_cmd(Range,5104),
  rec(5104).

%% @doc glNewList
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec newList(List, Mode) -> 'ok' when List :: integer(),Mode :: enum().
newList(List,Mode) when is_integer(List),is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(List,Mode,5105),
  ok.

%% @doc glBeginList
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec endList() -> 'ok'.
endList()  ->
  IF = get_interface(),
  IF:queue_cmd(5106),
  ok.

%% @doc glCallList
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec callList(List) -> 'ok' when List :: integer().
callList(List) when is_integer(List) ->
  IF = get_interface(),
  IF:queue_cmd(List,5107),
  ok.

%% @doc glCallLists
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec callLists(Lists) -> 'ok' when Lists :: [integer()].
callLists(Lists) when is_list(Lists) ->
  IF = get_interface(),
  N = length(Lists),
  IF:queue_cmd(N,Lists,5108),
  ok.

%% @doc glListBase
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec listBase(Base) -> 'ok' when Base :: integer().
listBase(Base) when is_integer(Base) ->
  IF = get_interface(),
  IF:queue_cmd(Base,5109),
  ok.

%% @doc glBegin
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec 'begin'(Mode) -> 'ok' when Mode :: enum().
'begin'(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5110),
  ok.

%% @doc glBegin
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec 'end'() -> 'ok'.
'end'()  ->
  IF = get_interface(),
  IF:queue_cmd(5111),
  ok.

%% @doc glVertex
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertex2d(X, Y) -> 'ok' when X :: float(),Y :: float().
vertex2d(X,Y) when is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5112),
  ok.

%% @doc glVertex
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertex2f(X, Y) -> 'ok' when X :: float(),Y :: float().
vertex2f(X,Y) when is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5113),
  ok.

%% @doc glVertex
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertex2i(X, Y) -> 'ok' when X :: integer(),Y :: integer().
vertex2i(X,Y) when is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5114),
  ok.

%% @doc glVertex
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertex2s(X, Y) -> 'ok' when X :: integer(),Y :: integer().
vertex2s(X,Y) when is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5115),
  ok.

%% @doc glVertex
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertex3d(X, Y, Z) -> 'ok' when X :: float(),Y :: float(),Z :: float().
vertex3d(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5116),
  ok.

%% @doc glVertex
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertex3f(X, Y, Z) -> 'ok' when X :: float(),Y :: float(),Z :: float().
vertex3f(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5117),
  ok.

%% @doc glVertex
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertex3i(X, Y, Z) -> 'ok' when X :: integer(),Y :: integer(),Z :: integer().
vertex3i(X,Y,Z) when is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5118),
  ok.

%% @doc glVertex
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertex3s(X, Y, Z) -> 'ok' when X :: integer(),Y :: integer(),Z :: integer().
vertex3s(X,Y,Z) when is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5119),
  ok.

%% @doc glVertex
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertex4d(X, Y, Z, W) -> 'ok' when X :: float(),Y :: float(),Z :: float(),W :: float().
vertex4d(X,Y,Z,W) when is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,W,5120),
  ok.

%% @doc glVertex
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertex4f(X, Y, Z, W) -> 'ok' when X :: float(),Y :: float(),Z :: float(),W :: float().
vertex4f(X,Y,Z,W) when is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,W,5121),
  ok.

%% @doc glVertex
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertex4i(X, Y, Z, W) -> 'ok' when X :: integer(),Y :: integer(),Z :: integer(),W :: integer().
vertex4i(X,Y,Z,W) when is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,W,5122),
  ok.

%% @doc glVertex
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertex4s(X, Y, Z, W) -> 'ok' when X :: integer(),Y :: integer(),Z :: integer(),W :: integer().
vertex4s(X,Y,Z,W) when is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,W,5123),
  ok.

%% @equiv vertex2d(X,Y)
-spec vertex2dv(V) -> 'ok' when V :: {X :: float(),Y :: float()}.
vertex2dv({X,Y}) ->  vertex2d(X,Y).
%% @equiv vertex2f(X,Y)
-spec vertex2fv(V) -> 'ok' when V :: {X :: float(),Y :: float()}.
vertex2fv({X,Y}) ->  vertex2f(X,Y).
%% @equiv vertex2i(X,Y)
-spec vertex2iv(V) -> 'ok' when V :: {X :: integer(),Y :: integer()}.
vertex2iv({X,Y}) ->  vertex2i(X,Y).
%% @equiv vertex2s(X,Y)
-spec vertex2sv(V) -> 'ok' when V :: {X :: integer(),Y :: integer()}.
vertex2sv({X,Y}) ->  vertex2s(X,Y).
%% @equiv vertex3d(X,Y,Z)
-spec vertex3dv(V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float()}.
vertex3dv({X,Y,Z}) ->  vertex3d(X,Y,Z).
%% @equiv vertex3f(X,Y,Z)
-spec vertex3fv(V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float()}.
vertex3fv({X,Y,Z}) ->  vertex3f(X,Y,Z).
%% @equiv vertex3i(X,Y,Z)
-spec vertex3iv(V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer()}.
vertex3iv({X,Y,Z}) ->  vertex3i(X,Y,Z).
%% @equiv vertex3s(X,Y,Z)
-spec vertex3sv(V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer()}.
vertex3sv({X,Y,Z}) ->  vertex3s(X,Y,Z).
%% @equiv vertex4d(X,Y,Z,W)
-spec vertex4dv(V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float(),W :: float()}.
vertex4dv({X,Y,Z,W}) ->  vertex4d(X,Y,Z,W).
%% @equiv vertex4f(X,Y,Z,W)
-spec vertex4fv(V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float(),W :: float()}.
vertex4fv({X,Y,Z,W}) ->  vertex4f(X,Y,Z,W).
%% @equiv vertex4i(X,Y,Z,W)
-spec vertex4iv(V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer(),W :: integer()}.
vertex4iv({X,Y,Z,W}) ->  vertex4i(X,Y,Z,W).
%% @equiv vertex4s(X,Y,Z,W)
-spec vertex4sv(V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer(),W :: integer()}.
vertex4sv({X,Y,Z,W}) ->  vertex4s(X,Y,Z,W).
%% @doc glNormal
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec normal3b(Nx, Ny, Nz) -> 'ok' when Nx :: integer(),Ny :: integer(),Nz :: integer().
normal3b(Nx,Ny,Nz) when is_integer(Nx),is_integer(Ny),is_integer(Nz) ->
  IF = get_interface(),
  IF:queue_cmd(Nx,Ny,Nz,5124),
  ok.

%% @doc glNormal
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec normal3d(Nx, Ny, Nz) -> 'ok' when Nx :: float(),Ny :: float(),Nz :: float().
normal3d(Nx,Ny,Nz) when is_float(Nx),is_float(Ny),is_float(Nz) ->
  IF = get_interface(),
  IF:queue_cmd(Nx,Ny,Nz,5125),
  ok.

%% @doc glNormal
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec normal3f(Nx, Ny, Nz) -> 'ok' when Nx :: float(),Ny :: float(),Nz :: float().
normal3f(Nx,Ny,Nz) when is_float(Nx),is_float(Ny),is_float(Nz) ->
  IF = get_interface(),
  IF:queue_cmd(Nx,Ny,Nz,5126),
  ok.

%% @doc glNormal
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec normal3i(Nx, Ny, Nz) -> 'ok' when Nx :: integer(),Ny :: integer(),Nz :: integer().
normal3i(Nx,Ny,Nz) when is_integer(Nx),is_integer(Ny),is_integer(Nz) ->
  IF = get_interface(),
  IF:queue_cmd(Nx,Ny,Nz,5127),
  ok.

%% @doc glNormal
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec normal3s(Nx, Ny, Nz) -> 'ok' when Nx :: integer(),Ny :: integer(),Nz :: integer().
normal3s(Nx,Ny,Nz) when is_integer(Nx),is_integer(Ny),is_integer(Nz) ->
  IF = get_interface(),
  IF:queue_cmd(Nx,Ny,Nz,5128),
  ok.

%% @equiv normal3b(Nx,Ny,Nz)
-spec normal3bv(V) -> 'ok' when V :: {Nx :: integer(),Ny :: integer(),Nz :: integer()}.
normal3bv({Nx,Ny,Nz}) ->  normal3b(Nx,Ny,Nz).
%% @equiv normal3d(Nx,Ny,Nz)
-spec normal3dv(V) -> 'ok' when V :: {Nx :: float(),Ny :: float(),Nz :: float()}.
normal3dv({Nx,Ny,Nz}) ->  normal3d(Nx,Ny,Nz).
%% @equiv normal3f(Nx,Ny,Nz)
-spec normal3fv(V) -> 'ok' when V :: {Nx :: float(),Ny :: float(),Nz :: float()}.
normal3fv({Nx,Ny,Nz}) ->  normal3f(Nx,Ny,Nz).
%% @equiv normal3i(Nx,Ny,Nz)
-spec normal3iv(V) -> 'ok' when V :: {Nx :: integer(),Ny :: integer(),Nz :: integer()}.
normal3iv({Nx,Ny,Nz}) ->  normal3i(Nx,Ny,Nz).
%% @equiv normal3s(Nx,Ny,Nz)
-spec normal3sv(V) -> 'ok' when V :: {Nx :: integer(),Ny :: integer(),Nz :: integer()}.
normal3sv({Nx,Ny,Nz}) ->  normal3s(Nx,Ny,Nz).
%% @doc glIndex
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec indexd(C) -> 'ok' when C :: float().
indexd(C) when is_float(C) ->
  IF = get_interface(),
  IF:queue_cmd(C,5129),
  ok.

%% @doc glIndex
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec indexf(C) -> 'ok' when C :: float().
indexf(C) when is_float(C) ->
  IF = get_interface(),
  IF:queue_cmd(C,5130),
  ok.

%% @doc glIndex
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec indexi(C) -> 'ok' when C :: integer().
indexi(C) when is_integer(C) ->
  IF = get_interface(),
  IF:queue_cmd(C,5131),
  ok.

%% @doc glIndex
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec indexs(C) -> 'ok' when C :: integer().
indexs(C) when is_integer(C) ->
  IF = get_interface(),
  IF:queue_cmd(C,5132),
  ok.

%% @doc glIndex
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec indexub(C) -> 'ok' when C :: integer().
indexub(C) when is_integer(C) ->
  IF = get_interface(),
  IF:queue_cmd(C,5133),
  ok.

%% @equiv indexd(C)
-spec indexdv(C) -> 'ok' when C :: {C :: float()}.
indexdv({C}) ->  indexd(C).
%% @equiv indexf(C)
-spec indexfv(C) -> 'ok' when C :: {C :: float()}.
indexfv({C}) ->  indexf(C).
%% @equiv indexi(C)
-spec indexiv(C) -> 'ok' when C :: {C :: integer()}.
indexiv({C}) ->  indexi(C).
%% @equiv indexs(C)
-spec indexsv(C) -> 'ok' when C :: {C :: integer()}.
indexsv({C}) ->  indexs(C).
%% @equiv indexub(C)
-spec indexubv(C) -> 'ok' when C :: {C :: integer()}.
indexubv({C}) ->  indexub(C).
%% @doc glColor
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec color3b(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
color3b(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5134),
  ok.

%% @doc glColor
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec color3d(Red, Green, Blue) -> 'ok' when Red :: float(),Green :: float(),Blue :: float().
color3d(Red,Green,Blue) when is_float(Red),is_float(Green),is_float(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5135),
  ok.

%% @doc glColor
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec color3f(Red, Green, Blue) -> 'ok' when Red :: float(),Green :: float(),Blue :: float().
color3f(Red,Green,Blue) when is_float(Red),is_float(Green),is_float(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5136),
  ok.

%% @doc glColor
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec color3i(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
color3i(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5137),
  ok.

%% @doc glColor
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec color3s(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
color3s(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5138),
  ok.

%% @doc glColor
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec color3ub(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
color3ub(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5139),
  ok.

%% @doc glColor
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec color3ui(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
color3ui(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5140),
  ok.

%% @doc glColor
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec color3us(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
color3us(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5141),
  ok.

%% @doc glColor
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec color4b(Red, Green, Blue, Alpha) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer(),Alpha :: integer().
color4b(Red,Green,Blue,Alpha) when is_integer(Red),is_integer(Green),is_integer(Blue),is_integer(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5142),
  ok.

%% @doc glColor
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec color4d(Red, Green, Blue, Alpha) -> 'ok' when Red :: float(),Green :: float(),Blue :: float(),Alpha :: float().
color4d(Red,Green,Blue,Alpha) when is_float(Red),is_float(Green),is_float(Blue),is_float(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5143),
  ok.

%% @doc glColor
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec color4f(Red, Green, Blue, Alpha) -> 'ok' when Red :: float(),Green :: float(),Blue :: float(),Alpha :: float().
color4f(Red,Green,Blue,Alpha) when is_float(Red),is_float(Green),is_float(Blue),is_float(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5144),
  ok.

%% @doc glColor
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec color4i(Red, Green, Blue, Alpha) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer(),Alpha :: integer().
color4i(Red,Green,Blue,Alpha) when is_integer(Red),is_integer(Green),is_integer(Blue),is_integer(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5145),
  ok.

%% @doc glColor
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec color4s(Red, Green, Blue, Alpha) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer(),Alpha :: integer().
color4s(Red,Green,Blue,Alpha) when is_integer(Red),is_integer(Green),is_integer(Blue),is_integer(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5146),
  ok.

%% @doc glColor
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec color4ub(Red, Green, Blue, Alpha) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer(),Alpha :: integer().
color4ub(Red,Green,Blue,Alpha) when is_integer(Red),is_integer(Green),is_integer(Blue),is_integer(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5147),
  ok.

%% @doc glColor
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec color4ui(Red, Green, Blue, Alpha) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer(),Alpha :: integer().
color4ui(Red,Green,Blue,Alpha) when is_integer(Red),is_integer(Green),is_integer(Blue),is_integer(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5148),
  ok.

%% @doc glColor
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec color4us(Red, Green, Blue, Alpha) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer(),Alpha :: integer().
color4us(Red,Green,Blue,Alpha) when is_integer(Red),is_integer(Green),is_integer(Blue),is_integer(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5149),
  ok.

%% @equiv color3b(Red,Green,Blue)
-spec color3bv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer()}.
color3bv({Red,Green,Blue}) ->  color3b(Red,Green,Blue).
%% @equiv color3d(Red,Green,Blue)
-spec color3dv(V) -> 'ok' when V :: {Red :: float(),Green :: float(),Blue :: float()}.
color3dv({Red,Green,Blue}) ->  color3d(Red,Green,Blue).
%% @equiv color3f(Red,Green,Blue)
-spec color3fv(V) -> 'ok' when V :: {Red :: float(),Green :: float(),Blue :: float()}.
color3fv({Red,Green,Blue}) ->  color3f(Red,Green,Blue).
%% @equiv color3i(Red,Green,Blue)
-spec color3iv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer()}.
color3iv({Red,Green,Blue}) ->  color3i(Red,Green,Blue).
%% @equiv color3s(Red,Green,Blue)
-spec color3sv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer()}.
color3sv({Red,Green,Blue}) ->  color3s(Red,Green,Blue).
%% @equiv color3ub(Red,Green,Blue)
-spec color3ubv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer()}.
color3ubv({Red,Green,Blue}) ->  color3ub(Red,Green,Blue).
%% @equiv color3ui(Red,Green,Blue)
-spec color3uiv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer()}.
color3uiv({Red,Green,Blue}) ->  color3ui(Red,Green,Blue).
%% @equiv color3us(Red,Green,Blue)
-spec color3usv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer()}.
color3usv({Red,Green,Blue}) ->  color3us(Red,Green,Blue).
%% @equiv color4b(Red,Green,Blue,Alpha)
-spec color4bv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer(),Alpha :: integer()}.
color4bv({Red,Green,Blue,Alpha}) ->  color4b(Red,Green,Blue,Alpha).
%% @equiv color4d(Red,Green,Blue,Alpha)
-spec color4dv(V) -> 'ok' when V :: {Red :: float(),Green :: float(),Blue :: float(),Alpha :: float()}.
color4dv({Red,Green,Blue,Alpha}) ->  color4d(Red,Green,Blue,Alpha).
%% @equiv color4f(Red,Green,Blue,Alpha)
-spec color4fv(V) -> 'ok' when V :: {Red :: float(),Green :: float(),Blue :: float(),Alpha :: float()}.
color4fv({Red,Green,Blue,Alpha}) ->  color4f(Red,Green,Blue,Alpha).
%% @equiv color4i(Red,Green,Blue,Alpha)
-spec color4iv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer(),Alpha :: integer()}.
color4iv({Red,Green,Blue,Alpha}) ->  color4i(Red,Green,Blue,Alpha).
%% @equiv color4s(Red,Green,Blue,Alpha)
-spec color4sv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer(),Alpha :: integer()}.
color4sv({Red,Green,Blue,Alpha}) ->  color4s(Red,Green,Blue,Alpha).
%% @equiv color4ub(Red,Green,Blue,Alpha)
-spec color4ubv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer(),Alpha :: integer()}.
color4ubv({Red,Green,Blue,Alpha}) ->  color4ub(Red,Green,Blue,Alpha).
%% @equiv color4ui(Red,Green,Blue,Alpha)
-spec color4uiv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer(),Alpha :: integer()}.
color4uiv({Red,Green,Blue,Alpha}) ->  color4ui(Red,Green,Blue,Alpha).
%% @equiv color4us(Red,Green,Blue,Alpha)
-spec color4usv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer(),Alpha :: integer()}.
color4usv({Red,Green,Blue,Alpha}) ->  color4us(Red,Green,Blue,Alpha).
%% @doc glTexCoord
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texCoord1d(S) -> 'ok' when S :: float().
texCoord1d(S) when is_float(S) ->
  IF = get_interface(),
  IF:queue_cmd(S,5150),
  ok.

%% @doc glTexCoord
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texCoord1f(S) -> 'ok' when S :: float().
texCoord1f(S) when is_float(S) ->
  IF = get_interface(),
  IF:queue_cmd(S,5151),
  ok.

%% @doc glTexCoord
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texCoord1i(S) -> 'ok' when S :: integer().
texCoord1i(S) when is_integer(S) ->
  IF = get_interface(),
  IF:queue_cmd(S,5152),
  ok.

%% @doc glTexCoord
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texCoord1s(S) -> 'ok' when S :: integer().
texCoord1s(S) when is_integer(S) ->
  IF = get_interface(),
  IF:queue_cmd(S,5153),
  ok.

%% @doc glTexCoord
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texCoord2d(S, T) -> 'ok' when S :: float(),T :: float().
texCoord2d(S,T) when is_float(S),is_float(T) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,5154),
  ok.

%% @doc glTexCoord
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texCoord2f(S, T) -> 'ok' when S :: float(),T :: float().
texCoord2f(S,T) when is_float(S),is_float(T) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,5155),
  ok.

%% @doc glTexCoord
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texCoord2i(S, T) -> 'ok' when S :: integer(),T :: integer().
texCoord2i(S,T) when is_integer(S),is_integer(T) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,5156),
  ok.

%% @doc glTexCoord
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texCoord2s(S, T) -> 'ok' when S :: integer(),T :: integer().
texCoord2s(S,T) when is_integer(S),is_integer(T) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,5157),
  ok.

%% @doc glTexCoord
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texCoord3d(S, T, R) -> 'ok' when S :: float(),T :: float(),R :: float().
texCoord3d(S,T,R) when is_float(S),is_float(T),is_float(R) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,R,5158),
  ok.

%% @doc glTexCoord
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texCoord3f(S, T, R) -> 'ok' when S :: float(),T :: float(),R :: float().
texCoord3f(S,T,R) when is_float(S),is_float(T),is_float(R) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,R,5159),
  ok.

%% @doc glTexCoord
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texCoord3i(S, T, R) -> 'ok' when S :: integer(),T :: integer(),R :: integer().
texCoord3i(S,T,R) when is_integer(S),is_integer(T),is_integer(R) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,R,5160),
  ok.

%% @doc glTexCoord
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texCoord3s(S, T, R) -> 'ok' when S :: integer(),T :: integer(),R :: integer().
texCoord3s(S,T,R) when is_integer(S),is_integer(T),is_integer(R) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,R,5161),
  ok.

%% @doc glTexCoord
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texCoord4d(S, T, R, Q) -> 'ok' when S :: float(),T :: float(),R :: float(),Q :: float().
texCoord4d(S,T,R,Q) when is_float(S),is_float(T),is_float(R),is_float(Q) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,R,Q,5162),
  ok.

%% @doc glTexCoord
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texCoord4f(S, T, R, Q) -> 'ok' when S :: float(),T :: float(),R :: float(),Q :: float().
texCoord4f(S,T,R,Q) when is_float(S),is_float(T),is_float(R),is_float(Q) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,R,Q,5163),
  ok.

%% @doc glTexCoord
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texCoord4i(S, T, R, Q) -> 'ok' when S :: integer(),T :: integer(),R :: integer(),Q :: integer().
texCoord4i(S,T,R,Q) when is_integer(S),is_integer(T),is_integer(R),is_integer(Q) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,R,Q,5164),
  ok.

%% @doc glTexCoord
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texCoord4s(S, T, R, Q) -> 'ok' when S :: integer(),T :: integer(),R :: integer(),Q :: integer().
texCoord4s(S,T,R,Q) when is_integer(S),is_integer(T),is_integer(R),is_integer(Q) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,R,Q,5165),
  ok.

%% @equiv texCoord1d(S)
-spec texCoord1dv(V) -> 'ok' when V :: {S :: float()}.
texCoord1dv({S}) ->  texCoord1d(S).
%% @equiv texCoord1f(S)
-spec texCoord1fv(V) -> 'ok' when V :: {S :: float()}.
texCoord1fv({S}) ->  texCoord1f(S).
%% @equiv texCoord1i(S)
-spec texCoord1iv(V) -> 'ok' when V :: {S :: integer()}.
texCoord1iv({S}) ->  texCoord1i(S).
%% @equiv texCoord1s(S)
-spec texCoord1sv(V) -> 'ok' when V :: {S :: integer()}.
texCoord1sv({S}) ->  texCoord1s(S).
%% @equiv texCoord2d(S,T)
-spec texCoord2dv(V) -> 'ok' when V :: {S :: float(),T :: float()}.
texCoord2dv({S,T}) ->  texCoord2d(S,T).
%% @equiv texCoord2f(S,T)
-spec texCoord2fv(V) -> 'ok' when V :: {S :: float(),T :: float()}.
texCoord2fv({S,T}) ->  texCoord2f(S,T).
%% @equiv texCoord2i(S,T)
-spec texCoord2iv(V) -> 'ok' when V :: {S :: integer(),T :: integer()}.
texCoord2iv({S,T}) ->  texCoord2i(S,T).
%% @equiv texCoord2s(S,T)
-spec texCoord2sv(V) -> 'ok' when V :: {S :: integer(),T :: integer()}.
texCoord2sv({S,T}) ->  texCoord2s(S,T).
%% @equiv texCoord3d(S,T,R)
-spec texCoord3dv(V) -> 'ok' when V :: {S :: float(),T :: float(),R :: float()}.
texCoord3dv({S,T,R}) ->  texCoord3d(S,T,R).
%% @equiv texCoord3f(S,T,R)
-spec texCoord3fv(V) -> 'ok' when V :: {S :: float(),T :: float(),R :: float()}.
texCoord3fv({S,T,R}) ->  texCoord3f(S,T,R).
%% @equiv texCoord3i(S,T,R)
-spec texCoord3iv(V) -> 'ok' when V :: {S :: integer(),T :: integer(),R :: integer()}.
texCoord3iv({S,T,R}) ->  texCoord3i(S,T,R).
%% @equiv texCoord3s(S,T,R)
-spec texCoord3sv(V) -> 'ok' when V :: {S :: integer(),T :: integer(),R :: integer()}.
texCoord3sv({S,T,R}) ->  texCoord3s(S,T,R).
%% @equiv texCoord4d(S,T,R,Q)
-spec texCoord4dv(V) -> 'ok' when V :: {S :: float(),T :: float(),R :: float(),Q :: float()}.
texCoord4dv({S,T,R,Q}) ->  texCoord4d(S,T,R,Q).
%% @equiv texCoord4f(S,T,R,Q)
-spec texCoord4fv(V) -> 'ok' when V :: {S :: float(),T :: float(),R :: float(),Q :: float()}.
texCoord4fv({S,T,R,Q}) ->  texCoord4f(S,T,R,Q).
%% @equiv texCoord4i(S,T,R,Q)
-spec texCoord4iv(V) -> 'ok' when V :: {S :: integer(),T :: integer(),R :: integer(),Q :: integer()}.
texCoord4iv({S,T,R,Q}) ->  texCoord4i(S,T,R,Q).
%% @equiv texCoord4s(S,T,R,Q)
-spec texCoord4sv(V) -> 'ok' when V :: {S :: integer(),T :: integer(),R :: integer(),Q :: integer()}.
texCoord4sv({S,T,R,Q}) ->  texCoord4s(S,T,R,Q).
%% @doc glRasterPos
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec rasterPos2d(X, Y) -> 'ok' when X :: float(),Y :: float().
rasterPos2d(X,Y) when is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5166),
  ok.

%% @doc glRasterPos
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec rasterPos2f(X, Y) -> 'ok' when X :: float(),Y :: float().
rasterPos2f(X,Y) when is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5167),
  ok.

%% @doc glRasterPos
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec rasterPos2i(X, Y) -> 'ok' when X :: integer(),Y :: integer().
rasterPos2i(X,Y) when is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5168),
  ok.

%% @doc glRasterPos
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec rasterPos2s(X, Y) -> 'ok' when X :: integer(),Y :: integer().
rasterPos2s(X,Y) when is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5169),
  ok.

%% @doc glRasterPos
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec rasterPos3d(X, Y, Z) -> 'ok' when X :: float(),Y :: float(),Z :: float().
rasterPos3d(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5170),
  ok.

%% @doc glRasterPos
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec rasterPos3f(X, Y, Z) -> 'ok' when X :: float(),Y :: float(),Z :: float().
rasterPos3f(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5171),
  ok.

%% @doc glRasterPos
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec rasterPos3i(X, Y, Z) -> 'ok' when X :: integer(),Y :: integer(),Z :: integer().
rasterPos3i(X,Y,Z) when is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5172),
  ok.

%% @doc glRasterPos
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec rasterPos3s(X, Y, Z) -> 'ok' when X :: integer(),Y :: integer(),Z :: integer().
rasterPos3s(X,Y,Z) when is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5173),
  ok.

%% @doc glRasterPos
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec rasterPos4d(X, Y, Z, W) -> 'ok' when X :: float(),Y :: float(),Z :: float(),W :: float().
rasterPos4d(X,Y,Z,W) when is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,W,5174),
  ok.

%% @doc glRasterPos
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec rasterPos4f(X, Y, Z, W) -> 'ok' when X :: float(),Y :: float(),Z :: float(),W :: float().
rasterPos4f(X,Y,Z,W) when is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,W,5175),
  ok.

%% @doc glRasterPos
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec rasterPos4i(X, Y, Z, W) -> 'ok' when X :: integer(),Y :: integer(),Z :: integer(),W :: integer().
rasterPos4i(X,Y,Z,W) when is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,W,5176),
  ok.

%% @doc glRasterPos
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec rasterPos4s(X, Y, Z, W) -> 'ok' when X :: integer(),Y :: integer(),Z :: integer(),W :: integer().
rasterPos4s(X,Y,Z,W) when is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,W,5177),
  ok.

%% @equiv rasterPos2d(X,Y)
-spec rasterPos2dv(V) -> 'ok' when V :: {X :: float(),Y :: float()}.
rasterPos2dv({X,Y}) ->  rasterPos2d(X,Y).
%% @equiv rasterPos2f(X,Y)
-spec rasterPos2fv(V) -> 'ok' when V :: {X :: float(),Y :: float()}.
rasterPos2fv({X,Y}) ->  rasterPos2f(X,Y).
%% @equiv rasterPos2i(X,Y)
-spec rasterPos2iv(V) -> 'ok' when V :: {X :: integer(),Y :: integer()}.
rasterPos2iv({X,Y}) ->  rasterPos2i(X,Y).
%% @equiv rasterPos2s(X,Y)
-spec rasterPos2sv(V) -> 'ok' when V :: {X :: integer(),Y :: integer()}.
rasterPos2sv({X,Y}) ->  rasterPos2s(X,Y).
%% @equiv rasterPos3d(X,Y,Z)
-spec rasterPos3dv(V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float()}.
rasterPos3dv({X,Y,Z}) ->  rasterPos3d(X,Y,Z).
%% @equiv rasterPos3f(X,Y,Z)
-spec rasterPos3fv(V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float()}.
rasterPos3fv({X,Y,Z}) ->  rasterPos3f(X,Y,Z).
%% @equiv rasterPos3i(X,Y,Z)
-spec rasterPos3iv(V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer()}.
rasterPos3iv({X,Y,Z}) ->  rasterPos3i(X,Y,Z).
%% @equiv rasterPos3s(X,Y,Z)
-spec rasterPos3sv(V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer()}.
rasterPos3sv({X,Y,Z}) ->  rasterPos3s(X,Y,Z).
%% @equiv rasterPos4d(X,Y,Z,W)
-spec rasterPos4dv(V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float(),W :: float()}.
rasterPos4dv({X,Y,Z,W}) ->  rasterPos4d(X,Y,Z,W).
%% @equiv rasterPos4f(X,Y,Z,W)
-spec rasterPos4fv(V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float(),W :: float()}.
rasterPos4fv({X,Y,Z,W}) ->  rasterPos4f(X,Y,Z,W).
%% @equiv rasterPos4i(X,Y,Z,W)
-spec rasterPos4iv(V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer(),W :: integer()}.
rasterPos4iv({X,Y,Z,W}) ->  rasterPos4i(X,Y,Z,W).
%% @equiv rasterPos4s(X,Y,Z,W)
-spec rasterPos4sv(V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer(),W :: integer()}.
rasterPos4sv({X,Y,Z,W}) ->  rasterPos4s(X,Y,Z,W).
%% @doc glRect
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec rectd(X1, Y1, X2, Y2) -> 'ok' when X1 :: float(),Y1 :: float(),X2 :: float(),Y2 :: float().
rectd(X1,Y1,X2,Y2) when is_float(X1),is_float(Y1),is_float(X2),is_float(Y2) ->
  IF = get_interface(),
  IF:queue_cmd(X1,Y1,X2,Y2,5178),
  ok.

%% @doc glRect
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec rectf(X1, Y1, X2, Y2) -> 'ok' when X1 :: float(),Y1 :: float(),X2 :: float(),Y2 :: float().
rectf(X1,Y1,X2,Y2) when is_float(X1),is_float(Y1),is_float(X2),is_float(Y2) ->
  IF = get_interface(),
  IF:queue_cmd(X1,Y1,X2,Y2,5179),
  ok.

%% @doc glRect
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec recti(X1, Y1, X2, Y2) -> 'ok' when X1 :: integer(),Y1 :: integer(),X2 :: integer(),Y2 :: integer().
recti(X1,Y1,X2,Y2) when is_integer(X1),is_integer(Y1),is_integer(X2),is_integer(Y2) ->
  IF = get_interface(),
  IF:queue_cmd(X1,Y1,X2,Y2,5180),
  ok.

%% @doc glRect
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec rects(X1, Y1, X2, Y2) -> 'ok' when X1 :: integer(),Y1 :: integer(),X2 :: integer(),Y2 :: integer().
rects(X1,Y1,X2,Y2) when is_integer(X1),is_integer(Y1),is_integer(X2),is_integer(Y2) ->
  IF = get_interface(),
  IF:queue_cmd(X1,Y1,X2,Y2,5181),
  ok.

%% @doc glRect
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec rectdv(V1, V2) -> 'ok' when V1 :: {float(),float()},V2 :: {float(),float()}.
rectdv(V1,V2) when tuple_size(V1) =:= 2,tuple_size(V2) =:= 2 ->
  IF = get_interface(),
  IF:queue_cmd(V1,V2,5182),
  ok.

%% @doc glRect
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec rectfv(V1, V2) -> 'ok' when V1 :: {float(),float()},V2 :: {float(),float()}.
rectfv(V1,V2) when tuple_size(V1) =:= 2,tuple_size(V2) =:= 2 ->
  IF = get_interface(),
  IF:queue_cmd(V1,V2,5183),
  ok.

%% @doc glRect
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec rectiv(V1, V2) -> 'ok' when V1 :: {integer(),integer()},V2 :: {integer(),integer()}.
rectiv(V1,V2) when tuple_size(V1) =:= 2,tuple_size(V2) =:= 2 ->
  IF = get_interface(),
  IF:queue_cmd(V1,V2,5184),
  ok.

%% @doc glRect
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec rectsv(V1, V2) -> 'ok' when V1 :: {integer(),integer()},V2 :: {integer(),integer()}.
rectsv(V1,V2) when tuple_size(V1) =:= 2,tuple_size(V2) =:= 2 ->
  IF = get_interface(),
  IF:queue_cmd(V1,V2,5185),
  ok.

%% @doc glVertexPointer
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexPointer(Size, Type, Stride, Ptr) -> 'ok' when Size :: integer(),Type :: enum(),Stride :: integer(),Ptr :: offset()|mem().
vertexPointer(Size,Type,Stride,Ptr) when is_integer(Size),is_integer(Type),is_integer(Stride),is_integer(Ptr) orelse is_tuple(Ptr) orelse is_binary(Ptr) ->
  IF = get_interface(),
  IF:queue_cmd(Size,Type,Stride,Ptr,5186),
  ok.

%% @doc glNormalPointer
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec normalPointer(Type, Stride, Ptr) -> 'ok' when Type :: enum(),Stride :: integer(),Ptr :: offset()|mem().
normalPointer(Type,Stride,Ptr) when is_integer(Type),is_integer(Stride),is_integer(Ptr) orelse is_tuple(Ptr) orelse is_binary(Ptr) ->
  IF = get_interface(),
  IF:queue_cmd(Type,Stride,Ptr,5188),
  ok.

%% @doc glColorPointer
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec colorPointer(Size, Type, Stride, Ptr) -> 'ok' when Size :: integer(),Type :: enum(),Stride :: integer(),Ptr :: offset()|mem().
colorPointer(Size,Type,Stride,Ptr) when is_integer(Size),is_integer(Type),is_integer(Stride),is_integer(Ptr) orelse is_tuple(Ptr) orelse is_binary(Ptr) ->
  IF = get_interface(),
  IF:queue_cmd(Size,Type,Stride,Ptr,5190),
  ok.

%% @doc glIndexPointer
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec indexPointer(Type, Stride, Ptr) -> 'ok' when Type :: enum(),Stride :: integer(),Ptr :: offset()|mem().
indexPointer(Type,Stride,Ptr) when is_integer(Type),is_integer(Stride),is_integer(Ptr) orelse is_tuple(Ptr) orelse is_binary(Ptr) ->
  IF = get_interface(),
  IF:queue_cmd(Type,Stride,Ptr,5192),
  ok.

%% @doc glTexCoordPointer
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texCoordPointer(Size, Type, Stride, Ptr) -> 'ok' when Size :: integer(),Type :: enum(),Stride :: integer(),Ptr :: offset()|mem().
texCoordPointer(Size,Type,Stride,Ptr) when is_integer(Size),is_integer(Type),is_integer(Stride),is_integer(Ptr) orelse is_tuple(Ptr) orelse is_binary(Ptr) ->
  IF = get_interface(),
  IF:queue_cmd(Size,Type,Stride,Ptr,5194),
  ok.

%% @doc glEdgeFlagPointer
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec edgeFlagPointer(Stride, Ptr) -> 'ok' when Stride :: integer(),Ptr :: offset()|mem().
edgeFlagPointer(Stride,Ptr) when is_integer(Stride),is_integer(Ptr) orelse is_tuple(Ptr) orelse is_binary(Ptr) ->
  IF = get_interface(),
  IF:queue_cmd(Stride,Ptr,5196),
  ok.

%% @doc glArrayElement
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec arrayElement(I) -> 'ok' when I :: integer().
arrayElement(I) when is_integer(I) ->
  IF = get_interface(),
  IF:queue_cmd(I,5198),
  ok.

%% @doc glDrawArrays
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec drawArrays(Mode, First, Count) -> 'ok' when Mode :: enum(),First :: integer(),Count :: integer().
drawArrays(Mode,First,Count) when is_integer(Mode),is_integer(First),is_integer(Count) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,First,Count,5199),
  ok.

%% @doc glDrawElements
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec drawElements(Mode, Count, Type, Indices) -> 'ok' when Mode :: enum(),Count :: integer(),Type :: enum(),Indices :: offset()|mem().
drawElements(Mode,Count,Type,Indices) when is_integer(Mode),is_integer(Count),is_integer(Type),is_integer(Indices) orelse is_tuple(Indices) orelse is_binary(Indices) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Count,Type,Indices,5200),
  ok.

%% @doc glInterleavedArrays
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec interleavedArrays(Format, Stride, Pointer) -> 'ok' when Format :: enum(),Stride :: integer(),Pointer :: offset()|mem().
interleavedArrays(Format,Stride,Pointer) when is_integer(Format),is_integer(Stride),is_integer(Pointer) orelse is_tuple(Pointer) orelse is_binary(Pointer) ->
  IF = get_interface(),
  IF:queue_cmd(Format,Stride,Pointer,5202),
  ok.

%% @doc glShadeModel
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec shadeModel(Mode) -> 'ok' when Mode :: enum().
shadeModel(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5204),
  ok.

%% @doc glLight
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec lightf(Light, Pname, Param) -> 'ok' when Light :: enum(),Pname :: enum(),Param :: float().
lightf(Light,Pname,Param) when is_integer(Light),is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Light,Pname,Param,5205),
  ok.

%% @doc glLight
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec lighti(Light, Pname, Param) -> 'ok' when Light :: enum(),Pname :: enum(),Param :: integer().
lighti(Light,Pname,Param) when is_integer(Light),is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Light,Pname,Param,5206),
  ok.

%% @doc glLight
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec lightfv(Light, Pname, Params) -> 'ok' when Light :: enum(),Pname :: enum(),Params :: tuple().
lightfv(Light,Pname,Params) when is_integer(Light),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Light,Pname,Params,5207),
  ok.

%% @doc glLight
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec lightiv(Light, Pname, Params) -> 'ok' when Light :: enum(),Pname :: enum(),Params :: tuple().
lightiv(Light,Pname,Params) when is_integer(Light),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Light,Pname,Params,5208),
  ok.

%% @doc glGetLight
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getLightfv(Light, Pname) -> {float(),float(),float(),float()} when Light :: enum(),Pname :: enum().
getLightfv(Light,Pname) when is_integer(Light),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Light,Pname,5209),
  rec(5209).

%% @doc glGetLight
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getLightiv(Light, Pname) -> {integer(),integer(),integer(),integer()} when Light :: enum(),Pname :: enum().
getLightiv(Light,Pname) when is_integer(Light),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Light,Pname,5210),
  rec(5210).

%% @doc glLightModel
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec lightModelf(Pname, Param) -> 'ok' when Pname :: enum(),Param :: float().
lightModelf(Pname,Param) when is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5211),
  ok.

%% @doc glLightModel
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec lightModeli(Pname, Param) -> 'ok' when Pname :: enum(),Param :: integer().
lightModeli(Pname,Param) when is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5212),
  ok.

%% @doc glLightModel
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec lightModelfv(Pname, Params) -> 'ok' when Pname :: enum(),Params :: tuple().
lightModelfv(Pname,Params) when is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Params,5213),
  ok.

%% @doc glLightModel
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec lightModeliv(Pname, Params) -> 'ok' when Pname :: enum(),Params :: tuple().
lightModeliv(Pname,Params) when is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Params,5214),
  ok.

%% @doc glMaterial
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec materialf(Face, Pname, Param) -> 'ok' when Face :: enum(),Pname :: enum(),Param :: float().
materialf(Face,Pname,Param) when is_integer(Face),is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Pname,Param,5215),
  ok.

%% @doc glMaterial
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec materiali(Face, Pname, Param) -> 'ok' when Face :: enum(),Pname :: enum(),Param :: integer().
materiali(Face,Pname,Param) when is_integer(Face),is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Pname,Param,5216),
  ok.

%% @doc glMaterial
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec materialfv(Face, Pname, Params) -> 'ok' when Face :: enum(),Pname :: enum(),Params :: tuple().
materialfv(Face,Pname,Params) when is_integer(Face),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Pname,Params,5217),
  ok.

%% @doc glMaterial
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec materialiv(Face, Pname, Params) -> 'ok' when Face :: enum(),Pname :: enum(),Params :: tuple().
materialiv(Face,Pname,Params) when is_integer(Face),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Pname,Params,5218),
  ok.

%% @doc glGetMaterial
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getMaterialfv(Face, Pname) -> {float(),float(),float(),float()} when Face :: enum(),Pname :: enum().
getMaterialfv(Face,Pname) when is_integer(Face),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Pname,5219),
  rec(5219).

%% @doc glGetMaterial
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getMaterialiv(Face, Pname) -> {integer(),integer(),integer(),integer()} when Face :: enum(),Pname :: enum().
getMaterialiv(Face,Pname) when is_integer(Face),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Pname,5220),
  rec(5220).

%% @doc glColorMaterial
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec colorMaterial(Face, Mode) -> 'ok' when Face :: enum(),Mode :: enum().
colorMaterial(Face,Mode) when is_integer(Face),is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Mode,5221),
  ok.

%% @doc glPixelZoom
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec pixelZoom(Xfactor, Yfactor) -> 'ok' when Xfactor :: float(),Yfactor :: float().
pixelZoom(Xfactor,Yfactor) when is_float(Xfactor),is_float(Yfactor) ->
  IF = get_interface(),
  IF:queue_cmd(Xfactor,Yfactor,5222),
  ok.

%% @doc glPixelStore
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec pixelStoref(Pname, Param) -> 'ok' when Pname :: enum(),Param :: float().
pixelStoref(Pname,Param) when is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5223),
  ok.

%% @doc glPixelStore
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec pixelStorei(Pname, Param) -> 'ok' when Pname :: enum(),Param :: integer().
pixelStorei(Pname,Param) when is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5224),
  ok.

%% @doc glPixelTransfer
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec pixelTransferf(Pname, Param) -> 'ok' when Pname :: enum(),Param :: float().
pixelTransferf(Pname,Param) when is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5225),
  ok.

%% @doc glPixelTransfer
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec pixelTransferi(Pname, Param) -> 'ok' when Pname :: enum(),Param :: integer().
pixelTransferi(Pname,Param) when is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5226),
  ok.

%% @doc glPixelMap
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec pixelMapfv(Map, Mapsize, Values) -> 'ok' when Map :: enum(),Mapsize :: integer(),Values :: binary().
pixelMapfv(Map,Mapsize,Values) when is_integer(Map),is_integer(Mapsize),is_binary(Values) ->
  IF = get_interface(),
  IF:queue_cmd(Map,Mapsize,Values,5227),
  ok.

%% @doc glPixelMap
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec pixelMapuiv(Map, Mapsize, Values) -> 'ok' when Map :: enum(),Mapsize :: integer(),Values :: binary().
pixelMapuiv(Map,Mapsize,Values) when is_integer(Map),is_integer(Mapsize),is_binary(Values) ->
  IF = get_interface(),
  IF:queue_cmd(Map,Mapsize,Values,5228),
  ok.

%% @doc glPixelMap
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec pixelMapusv(Map, Mapsize, Values) -> 'ok' when Map :: enum(),Mapsize :: integer(),Values :: binary().
pixelMapusv(Map,Mapsize,Values) when is_integer(Map),is_integer(Mapsize),is_binary(Values) ->
  IF = get_interface(),
  IF:queue_cmd(Map,Mapsize,Values,5229),
  ok.

%% @doc glGetPixelMap
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getPixelMapfv(Map, Values) -> 'ok' when Map :: enum(),Values :: mem().
getPixelMapfv(Map,Values) when is_integer(Map),is_tuple(Values) orelse is_binary(Values) ->
  IF = get_interface(),
  IF:queue_cmd(Map,Values,5230),
  rec(5230).

%% @doc glGetPixelMap
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getPixelMapuiv(Map, Values) -> 'ok' when Map :: enum(),Values :: mem().
getPixelMapuiv(Map,Values) when is_integer(Map),is_tuple(Values) orelse is_binary(Values) ->
  IF = get_interface(),
  IF:queue_cmd(Map,Values,5231),
  rec(5231).

%% @doc glGetPixelMap
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getPixelMapusv(Map, Values) -> 'ok' when Map :: enum(),Values :: mem().
getPixelMapusv(Map,Values) when is_integer(Map),is_tuple(Values) orelse is_binary(Values) ->
  IF = get_interface(),
  IF:queue_cmd(Map,Values,5232),
  rec(5232).

%% @doc glBitmap
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec bitmap(Width, Height, Xorig, Yorig, Xmove, Ymove, Bitmap) -> 'ok' when Width :: integer(),Height :: integer(),Xorig :: float(),Yorig :: float(),Xmove :: float(),Ymove :: float(),Bitmap :: offset()|mem().
bitmap(Width,Height,Xorig,Yorig,Xmove,Ymove,Bitmap) when is_integer(Width),is_integer(Height),is_float(Xorig),is_float(Yorig),is_float(Xmove),is_float(Ymove),is_integer(Bitmap) orelse is_tuple(Bitmap) orelse is_binary(Bitmap) ->
  IF = get_interface(),
  IF:queue_cmd(Width,Height,Xorig,Yorig,Xmove,Ymove,Bitmap,5233),
  ok.

%% @doc glReadPixels
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec readPixels(X, Y, Width, Height, Format, Type, Pixels) -> 'ok' when X :: integer(),Y :: integer(),Width :: integer(),Height :: integer(),Format :: enum(),Type :: enum(),Pixels :: mem().
readPixels(X,Y,Width,Height,Format,Type,Pixels) when is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height),is_integer(Format),is_integer(Type),is_tuple(Pixels) orelse is_binary(Pixels) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Width,Height,Format,Type,Pixels,5235),
  rec(5235).

%% @doc glDrawPixels
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec drawPixels(Width, Height, Format, Type, Pixels) -> 'ok' when Width :: integer(),Height :: integer(),Format :: enum(),Type :: enum(),Pixels :: offset()|mem().
drawPixels(Width,Height,Format,Type,Pixels) when is_integer(Width),is_integer(Height),is_integer(Format),is_integer(Type),is_integer(Pixels) orelse is_tuple(Pixels) orelse is_binary(Pixels) ->
  IF = get_interface(),
  IF:queue_cmd(Width,Height,Format,Type,Pixels,5236),
  ok.

%% @doc glCopyPixels
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec copyPixels(X, Y, Width, Height, Type) -> 'ok' when X :: integer(),Y :: integer(),Width :: integer(),Height :: integer(),Type :: enum().
copyPixels(X,Y,Width,Height,Type) when is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height),is_integer(Type) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Width,Height,Type,5238),
  ok.

%% @doc glStencilFunc
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec stencilFunc(Func, Ref, Mask) -> 'ok' when Func :: enum(),Ref :: integer(),Mask :: integer().
stencilFunc(Func,Ref,Mask) when is_integer(Func),is_integer(Ref),is_integer(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(Func,Ref,Mask,5239),
  ok.

%% @doc glStencilMask
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec stencilMask(Mask) -> 'ok' when Mask :: integer().
stencilMask(Mask) when is_integer(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(Mask,5240),
  ok.

%% @doc glStencilOp
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec stencilOp(Fail, Zfail, Zpass) -> 'ok' when Fail :: enum(),Zfail :: enum(),Zpass :: enum().
stencilOp(Fail,Zfail,Zpass) when is_integer(Fail),is_integer(Zfail),is_integer(Zpass) ->
  IF = get_interface(),
  IF:queue_cmd(Fail,Zfail,Zpass,5241),
  ok.

%% @doc glClearStencil
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec clearStencil(S) -> 'ok' when S :: integer().
clearStencil(S) when is_integer(S) ->
  IF = get_interface(),
  IF:queue_cmd(S,5242),
  ok.

%% @doc glTexGen
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texGend(Coord, Pname, Param) -> 'ok' when Coord :: enum(),Pname :: enum(),Param :: float().
texGend(Coord,Pname,Param) when is_integer(Coord),is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,Pname,Param,5243),
  ok.

%% @doc glTexGen
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texGenf(Coord, Pname, Param) -> 'ok' when Coord :: enum(),Pname :: enum(),Param :: float().
texGenf(Coord,Pname,Param) when is_integer(Coord),is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,Pname,Param,5244),
  ok.

%% @doc glTexGen
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texGeni(Coord, Pname, Param) -> 'ok' when Coord :: enum(),Pname :: enum(),Param :: integer().
texGeni(Coord,Pname,Param) when is_integer(Coord),is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,Pname,Param,5245),
  ok.

%% @doc glTexGen
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texGendv(Coord, Pname, Params) -> 'ok' when Coord :: enum(),Pname :: enum(),Params :: tuple().
texGendv(Coord,Pname,Params) when is_integer(Coord),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,Pname,Params,5246),
  ok.

%% @doc glTexGen
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texGenfv(Coord, Pname, Params) -> 'ok' when Coord :: enum(),Pname :: enum(),Params :: tuple().
texGenfv(Coord,Pname,Params) when is_integer(Coord),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,Pname,Params,5247),
  ok.

%% @doc glTexGen
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texGeniv(Coord, Pname, Params) -> 'ok' when Coord :: enum(),Pname :: enum(),Params :: tuple().
texGeniv(Coord,Pname,Params) when is_integer(Coord),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,Pname,Params,5248),
  ok.

%% @doc glGetTexGen
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getTexGendv(Coord, Pname) -> {float(),float(),float(),float()} when Coord :: enum(),Pname :: enum().
getTexGendv(Coord,Pname) when is_integer(Coord),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,Pname,5249),
  rec(5249).

%% @doc glGetTexGen
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getTexGenfv(Coord, Pname) -> {float(),float(),float(),float()} when Coord :: enum(),Pname :: enum().
getTexGenfv(Coord,Pname) when is_integer(Coord),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,Pname,5250),
  rec(5250).

%% @doc glGetTexGen
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getTexGeniv(Coord, Pname) -> {integer(),integer(),integer(),integer()} when Coord :: enum(),Pname :: enum().
getTexGeniv(Coord,Pname) when is_integer(Coord),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,Pname,5251),
  rec(5251).

%% @doc glTexEnvf
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texEnvf(Target, Pname, Param) -> 'ok' when Target :: enum(),Pname :: enum(),Param :: float().
texEnvf(Target,Pname,Param) when is_integer(Target),is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Param,5252),
  ok.

%% @doc glTexEnvi
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texEnvi(Target, Pname, Param) -> 'ok' when Target :: enum(),Pname :: enum(),Param :: integer().
texEnvi(Target,Pname,Param) when is_integer(Target),is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Param,5253),
  ok.

%% @doc glTexEnv
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texEnvfv(Target, Pname, Params) -> 'ok' when Target :: enum(),Pname :: enum(),Params :: tuple().
texEnvfv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5254),
  ok.

%% @doc glTexEnv
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texEnviv(Target, Pname, Params) -> 'ok' when Target :: enum(),Pname :: enum(),Params :: tuple().
texEnviv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5255),
  ok.

%% @doc glGetTexEnv
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getTexEnvfv(Target, Pname) -> {float(),float(),float(),float()} when Target :: enum(),Pname :: enum().
getTexEnvfv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5256),
  rec(5256).

%% @doc glGetTexEnv
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getTexEnviv(Target, Pname) -> {integer(),integer(),integer(),integer()} when Target :: enum(),Pname :: enum().
getTexEnviv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5257),
  rec(5257).

%% @doc glTexParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texParameterf(Target, Pname, Param) -> 'ok' when Target :: enum(),Pname :: enum(),Param :: float().
texParameterf(Target,Pname,Param) when is_integer(Target),is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Param,5258),
  ok.

%% @doc glTexParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texParameteri(Target, Pname, Param) -> 'ok' when Target :: enum(),Pname :: enum(),Param :: integer().
texParameteri(Target,Pname,Param) when is_integer(Target),is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Param,5259),
  ok.

%% @doc glTexParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texParameterfv(Target, Pname, Params) -> 'ok' when Target :: enum(),Pname :: enum(),Params :: tuple().
texParameterfv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5260),
  ok.

%% @doc glTexParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texParameteriv(Target, Pname, Params) -> 'ok' when Target :: enum(),Pname :: enum(),Params :: tuple().
texParameteriv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5261),
  ok.

%% @doc glGetTexParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getTexParameterfv(Target, Pname) -> {float(),float(),float(),float()} when Target :: enum(),Pname :: enum().
getTexParameterfv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5262),
  rec(5262).

%% @doc glGetTexParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getTexParameteriv(Target, Pname) -> {integer(),integer(),integer(),integer()} when Target :: enum(),Pname :: enum().
getTexParameteriv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5263),
  rec(5263).

%% @doc glGetTexLevelParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getTexLevelParameterfv(Target, Level, Pname) -> {float()} when Target :: enum(),Level :: integer(),Pname :: enum().
getTexLevelParameterfv(Target,Level,Pname) when is_integer(Target),is_integer(Level),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Pname,5264),
  rec(5264).

%% @doc glGetTexLevelParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getTexLevelParameteriv(Target, Level, Pname) -> {integer()} when Target :: enum(),Level :: integer(),Pname :: enum().
getTexLevelParameteriv(Target,Level,Pname) when is_integer(Target),is_integer(Level),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Pname,5265),
  rec(5265).

%% @doc glTexImage1D
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texImage1D(Target, Level, InternalFormat, Width, Border, Format, Type, Pixels) -> 'ok' when Target :: enum(),Level :: integer(),InternalFormat :: integer(),Width :: integer(),Border :: integer(),Format :: enum(),Type :: enum(),Pixels :: offset()|mem().
texImage1D(Target,Level,InternalFormat,Width,Border,Format,Type,Pixels) when is_integer(Target),is_integer(Level),is_integer(InternalFormat),is_integer(Width),is_integer(Border),is_integer(Format),is_integer(Type),is_integer(Pixels) orelse is_tuple(Pixels) orelse is_binary(Pixels) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,InternalFormat,Width,Border,Format,Type,Pixels,5266),
  ok.

%% @doc glTexImage2D
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texImage2D(Target, Level, InternalFormat, Width, Height, Border, Format, Type, Pixels) -> 'ok' when Target :: enum(),Level :: integer(),InternalFormat :: integer(),Width :: integer(),Height :: integer(),Border :: integer(),Format :: enum(),Type :: enum(),Pixels :: offset()|mem().
texImage2D(Target,Level,InternalFormat,Width,Height,Border,Format,Type,Pixels) when is_integer(Target),is_integer(Level),is_integer(InternalFormat),is_integer(Width),is_integer(Height),is_integer(Border),is_integer(Format),is_integer(Type),is_integer(Pixels) orelse is_tuple(Pixels) orelse is_binary(Pixels) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,InternalFormat,Width,Height,Border,Format,Type,Pixels,5268),
  ok.

%% @doc glGetTexImage
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getTexImage(Target, Level, Format, Type, Pixels) -> 'ok' when Target :: enum(),Level :: integer(),Format :: enum(),Type :: enum(),Pixels :: mem().
getTexImage(Target,Level,Format,Type,Pixels) when is_integer(Target),is_integer(Level),is_integer(Format),is_integer(Type),is_tuple(Pixels) orelse is_binary(Pixels) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Format,Type,Pixels,5270),
  rec(5270).

%% @doc glGenTextures
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec genTextures(N) -> [integer()] when N :: integer().
genTextures(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5271),
  rec(5271).

%% @doc glDeleteTextures
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec deleteTextures(Textures) -> 'ok' when Textures :: [integer()].
deleteTextures(Textures) when is_list(Textures) ->
  IF = get_interface(),
  N = length(Textures),
  IF:queue_cmd(N,Textures,5272),
  ok.

%% @doc glBindTexture
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec bindTexture(Target, Texture) -> 'ok' when Target :: enum(),Texture :: integer().
bindTexture(Target,Texture) when is_integer(Target),is_integer(Texture) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Texture,5273),
  ok.

%% @doc glPrioritizeTextures
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec prioritizeTextures(Textures, Priorities) -> 'ok' when Textures :: [integer()],Priorities :: [clamp()].
prioritizeTextures(Textures,Priorities) when is_list(Textures),is_list(Priorities) ->
  IF = get_interface(),
  N = length(Textures),
  IF:queue_cmd(N,Textures,Priorities,5274),
  ok.

%% @doc glAreTexturesResident
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec areTexturesResident(Textures) -> {0|1,Residences :: [0|1]} when Textures :: [integer()].
areTexturesResident(Textures) when is_list(Textures) ->
  IF = get_interface(),
  N = length(Textures),
  IF:queue_cmd(N,Textures,5275),
  rec(5275).

%% @doc glIsTexture
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec isTexture(Texture) -> 0|1 when Texture :: integer().
isTexture(Texture) when is_integer(Texture) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,5276),
  rec(5276).

%% @doc glTexSubImage
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texSubImage1D(Target, Level, Xoffset, Width, Format, Type, Pixels) -> 'ok' when Target :: enum(),Level :: integer(),Xoffset :: integer(),Width :: integer(),Format :: enum(),Type :: enum(),Pixels :: offset()|mem().
texSubImage1D(Target,Level,Xoffset,Width,Format,Type,Pixels) when is_integer(Target),is_integer(Level),is_integer(Xoffset),is_integer(Width),is_integer(Format),is_integer(Type),is_integer(Pixels) orelse is_tuple(Pixels) orelse is_binary(Pixels) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Xoffset,Width,Format,Type,Pixels,5277),
  ok.

%% @doc glTexSubImage
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texSubImage2D(Target, Level, Xoffset, Yoffset, Width, Height, Format, Type, Pixels) -> 'ok' when Target :: enum(),Level :: integer(),Xoffset :: integer(),Yoffset :: integer(),Width :: integer(),Height :: integer(),Format :: enum(),Type :: enum(),Pixels :: offset()|mem().
texSubImage2D(Target,Level,Xoffset,Yoffset,Width,Height,Format,Type,Pixels) when is_integer(Target),is_integer(Level),is_integer(Xoffset),is_integer(Yoffset),is_integer(Width),is_integer(Height),is_integer(Format),is_integer(Type),is_integer(Pixels) orelse is_tuple(Pixels) orelse is_binary(Pixels) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Xoffset,Yoffset,Width,Height,Format,Type,Pixels,5279),
  ok.

%% @doc glCopyTexImage1D
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec copyTexImage1D(Target, Level, Internalformat, X, Y, Width, Border) -> 'ok' when Target :: enum(),Level :: integer(),Internalformat :: enum(),X :: integer(),Y :: integer(),Width :: integer(),Border :: integer().
copyTexImage1D(Target,Level,Internalformat,X,Y,Width,Border) when is_integer(Target),is_integer(Level),is_integer(Internalformat),is_integer(X),is_integer(Y),is_integer(Width),is_integer(Border) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Internalformat,X,Y,Width,Border,5281),
  ok.

%% @doc glCopyTexImage2D
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec copyTexImage2D(Target, Level, Internalformat, X, Y, Width, Height, Border) -> 'ok' when Target :: enum(),Level :: integer(),Internalformat :: enum(),X :: integer(),Y :: integer(),Width :: integer(),Height :: integer(),Border :: integer().
copyTexImage2D(Target,Level,Internalformat,X,Y,Width,Height,Border) when is_integer(Target),is_integer(Level),is_integer(Internalformat),is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height),is_integer(Border) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Internalformat,X,Y,Width,Height,Border,5282),
  ok.

%% @doc glCopyTexSubImage1D
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec copyTexSubImage1D(Target, Level, Xoffset, X, Y, Width) -> 'ok' when Target :: enum(),Level :: integer(),Xoffset :: integer(),X :: integer(),Y :: integer(),Width :: integer().
copyTexSubImage1D(Target,Level,Xoffset,X,Y,Width) when is_integer(Target),is_integer(Level),is_integer(Xoffset),is_integer(X),is_integer(Y),is_integer(Width) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Xoffset,X,Y,Width,5283),
  ok.

%% @doc glCopyTexSubImage2D
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec copyTexSubImage2D(Target, Level, Xoffset, Yoffset, X, Y, Width, Height) -> 'ok' when Target :: enum(),Level :: integer(),Xoffset :: integer(),Yoffset :: integer(),X :: integer(),Y :: integer(),Width :: integer(),Height :: integer().
copyTexSubImage2D(Target,Level,Xoffset,Yoffset,X,Y,Width,Height) when is_integer(Target),is_integer(Level),is_integer(Xoffset),is_integer(Yoffset),is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Xoffset,Yoffset,X,Y,Width,Height,5284),
  ok.

%% @doc glMap
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec map1d(Target, U1, U2, Stride, Order, Points) -> 'ok' when Target :: enum(),U1 :: float(),U2 :: float(),Stride :: integer(),Order :: integer(),Points :: binary().
map1d(Target,U1,U2,Stride,Order,Points) when is_integer(Target),is_float(U1),is_float(U2),is_integer(Stride),is_integer(Order),is_binary(Points) ->
  IF = get_interface(),
  IF:queue_cmd(Target,U1,U2,Stride,Order,Points,5285),
  ok.

%% @doc glMap
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec map1f(Target, U1, U2, Stride, Order, Points) -> 'ok' when Target :: enum(),U1 :: float(),U2 :: float(),Stride :: integer(),Order :: integer(),Points :: binary().
map1f(Target,U1,U2,Stride,Order,Points) when is_integer(Target),is_float(U1),is_float(U2),is_integer(Stride),is_integer(Order),is_binary(Points) ->
  IF = get_interface(),
  IF:queue_cmd(Target,U1,U2,Stride,Order,Points,5286),
  ok.

%% @doc glMap
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec map2d(Target, U1, U2, Ustride, Uorder, V1, V2, Vstride, Vorder, Points) -> 'ok' when Target :: enum(),U1 :: float(),U2 :: float(),Ustride :: integer(),Uorder :: integer(),V1 :: float(),V2 :: float(),Vstride :: integer(),Vorder :: integer(),Points :: binary().
map2d(Target,U1,U2,Ustride,Uorder,V1,V2,Vstride,Vorder,Points) when is_integer(Target),is_float(U1),is_float(U2),is_integer(Ustride),is_integer(Uorder),is_float(V1),is_float(V2),is_integer(Vstride),is_integer(Vorder),is_binary(Points) ->
  IF = get_interface(),
  IF:queue_cmd(Target,U1,U2,Ustride,Uorder,V1,V2,Vstride,Vorder,Points,5287),
  ok.

%% @doc glMap
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec map2f(Target, U1, U2, Ustride, Uorder, V1, V2, Vstride, Vorder, Points) -> 'ok' when Target :: enum(),U1 :: float(),U2 :: float(),Ustride :: integer(),Uorder :: integer(),V1 :: float(),V2 :: float(),Vstride :: integer(),Vorder :: integer(),Points :: binary().
map2f(Target,U1,U2,Ustride,Uorder,V1,V2,Vstride,Vorder,Points) when is_integer(Target),is_float(U1),is_float(U2),is_integer(Ustride),is_integer(Uorder),is_float(V1),is_float(V2),is_integer(Vstride),is_integer(Vorder),is_binary(Points) ->
  IF = get_interface(),
  IF:queue_cmd(Target,U1,U2,Ustride,Uorder,V1,V2,Vstride,Vorder,Points,5288),
  ok.

%% @doc glGetMap
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getMapdv(Target, Query, V) -> 'ok' when Target :: enum(),Query :: enum(),V :: mem().
getMapdv(Target,Query,V) when is_integer(Target),is_integer(Query),is_tuple(V) orelse is_binary(V) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Query,V,5289),
  rec(5289).

%% @doc glGetMap
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getMapfv(Target, Query, V) -> 'ok' when Target :: enum(),Query :: enum(),V :: mem().
getMapfv(Target,Query,V) when is_integer(Target),is_integer(Query),is_tuple(V) orelse is_binary(V) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Query,V,5290),
  rec(5290).

%% @doc glGetMap
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getMapiv(Target, Query, V) -> 'ok' when Target :: enum(),Query :: enum(),V :: mem().
getMapiv(Target,Query,V) when is_integer(Target),is_integer(Query),is_tuple(V) orelse is_binary(V) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Query,V,5291),
  rec(5291).

%% @doc glEvalCoord
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec evalCoord1d(U) -> 'ok' when U :: float().
evalCoord1d(U) when is_float(U) ->
  IF = get_interface(),
  IF:queue_cmd(U,5292),
  ok.

%% @doc glEvalCoord
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec evalCoord1f(U) -> 'ok' when U :: float().
evalCoord1f(U) when is_float(U) ->
  IF = get_interface(),
  IF:queue_cmd(U,5293),
  ok.

%% @equiv evalCoord1d(U)
-spec evalCoord1dv(U) -> 'ok' when U :: {U :: float()}.
evalCoord1dv({U}) ->  evalCoord1d(U).
%% @equiv evalCoord1f(U)
-spec evalCoord1fv(U) -> 'ok' when U :: {U :: float()}.
evalCoord1fv({U}) ->  evalCoord1f(U).
%% @doc glEvalCoord
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec evalCoord2d(U, V) -> 'ok' when U :: float(),V :: float().
evalCoord2d(U,V) when is_float(U),is_float(V) ->
  IF = get_interface(),
  IF:queue_cmd(U,V,5294),
  ok.

%% @doc glEvalCoord
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec evalCoord2f(U, V) -> 'ok' when U :: float(),V :: float().
evalCoord2f(U,V) when is_float(U),is_float(V) ->
  IF = get_interface(),
  IF:queue_cmd(U,V,5295),
  ok.

%% @equiv evalCoord2d(U,V)
-spec evalCoord2dv(U) -> 'ok' when U :: {U :: float(),V :: float()}.
evalCoord2dv({U,V}) ->  evalCoord2d(U,V).
%% @equiv evalCoord2f(U,V)
-spec evalCoord2fv(U) -> 'ok' when U :: {U :: float(),V :: float()}.
evalCoord2fv({U,V}) ->  evalCoord2f(U,V).
%% @doc glMapGrid
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec mapGrid1d(Un, U1, U2) -> 'ok' when Un :: integer(),U1 :: float(),U2 :: float().
mapGrid1d(Un,U1,U2) when is_integer(Un),is_float(U1),is_float(U2) ->
  IF = get_interface(),
  IF:queue_cmd(Un,U1,U2,5296),
  ok.

%% @doc glMapGrid
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec mapGrid1f(Un, U1, U2) -> 'ok' when Un :: integer(),U1 :: float(),U2 :: float().
mapGrid1f(Un,U1,U2) when is_integer(Un),is_float(U1),is_float(U2) ->
  IF = get_interface(),
  IF:queue_cmd(Un,U1,U2,5297),
  ok.

%% @doc glMapGrid
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec mapGrid2d(Un, U1, U2, Vn, V1, V2) -> 'ok' when Un :: integer(),U1 :: float(),U2 :: float(),Vn :: integer(),V1 :: float(),V2 :: float().
mapGrid2d(Un,U1,U2,Vn,V1,V2) when is_integer(Un),is_float(U1),is_float(U2),is_integer(Vn),is_float(V1),is_float(V2) ->
  IF = get_interface(),
  IF:queue_cmd(Un,U1,U2,Vn,V1,V2,5298),
  ok.

%% @doc glMapGrid
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec mapGrid2f(Un, U1, U2, Vn, V1, V2) -> 'ok' when Un :: integer(),U1 :: float(),U2 :: float(),Vn :: integer(),V1 :: float(),V2 :: float().
mapGrid2f(Un,U1,U2,Vn,V1,V2) when is_integer(Un),is_float(U1),is_float(U2),is_integer(Vn),is_float(V1),is_float(V2) ->
  IF = get_interface(),
  IF:queue_cmd(Un,U1,U2,Vn,V1,V2,5299),
  ok.

%% @doc glEvalPoint
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec evalPoint1(I) -> 'ok' when I :: integer().
evalPoint1(I) when is_integer(I) ->
  IF = get_interface(),
  IF:queue_cmd(I,5300),
  ok.

%% @doc glEvalPoint
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec evalPoint2(I, J) -> 'ok' when I :: integer(),J :: integer().
evalPoint2(I,J) when is_integer(I),is_integer(J) ->
  IF = get_interface(),
  IF:queue_cmd(I,J,5301),
  ok.

%% @doc glEvalMesh
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec evalMesh1(Mode, I1, I2) -> 'ok' when Mode :: enum(),I1 :: integer(),I2 :: integer().
evalMesh1(Mode,I1,I2) when is_integer(Mode),is_integer(I1),is_integer(I2) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,I1,I2,5302),
  ok.

%% @doc glEvalMesh
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec evalMesh2(Mode, I1, I2, J1, J2) -> 'ok' when Mode :: enum(),I1 :: integer(),I2 :: integer(),J1 :: integer(),J2 :: integer().
evalMesh2(Mode,I1,I2,J1,J2) when is_integer(Mode),is_integer(I1),is_integer(I2),is_integer(J1),is_integer(J2) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,I1,I2,J1,J2,5303),
  ok.

%% @doc glFog
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec fogf(Pname, Param) -> 'ok' when Pname :: enum(),Param :: float().
fogf(Pname,Param) when is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5304),
  ok.

%% @doc glFog
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec fogi(Pname, Param) -> 'ok' when Pname :: enum(),Param :: integer().
fogi(Pname,Param) when is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5305),
  ok.

%% @doc glFog
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec fogfv(Pname, Params) -> 'ok' when Pname :: enum(),Params :: tuple().
fogfv(Pname,Params) when is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Params,5306),
  ok.

%% @doc glFog
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec fogiv(Pname, Params) -> 'ok' when Pname :: enum(),Params :: tuple().
fogiv(Pname,Params) when is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Params,5307),
  ok.

%% @doc glFeedbackBuffer
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec feedbackBuffer(Size, Type, Buffer) -> 'ok' when Size :: integer(),Type :: enum(),Buffer :: mem().
feedbackBuffer(Size,Type,Buffer) when is_integer(Size),is_integer(Type),is_tuple(Buffer) orelse is_binary(Buffer) ->
  IF = get_interface(),
  IF:queue_cmd(Size,Type,Buffer,5308),
  rec(5308).

%% @doc glPassThrough
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec passThrough(Token) -> 'ok' when Token :: float().
passThrough(Token) when is_float(Token) ->
  IF = get_interface(),
  IF:queue_cmd(Token,5309),
  ok.

%% @doc glSelectBuffer
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec selectBuffer(Size, Buffer) -> 'ok' when Size :: integer(),Buffer :: mem().
selectBuffer(Size,Buffer) when is_integer(Size),is_tuple(Buffer) orelse is_binary(Buffer) ->
  IF = get_interface(),
  IF:queue_cmd(Size,Buffer,5310),
  rec(5310).

%% @doc glInitNames
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec initNames() -> 'ok'.
initNames()  ->
  IF = get_interface(),
  IF:queue_cmd(5311),
  ok.

%% @doc glLoadName
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec loadName(Name) -> 'ok' when Name :: integer().
loadName(Name) when is_integer(Name) ->
  IF = get_interface(),
  IF:queue_cmd(Name,5312),
  ok.

%% @doc glPushName
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec pushName(Name) -> 'ok' when Name :: integer().
pushName(Name) when is_integer(Name) ->
  IF = get_interface(),
  IF:queue_cmd(Name,5313),
  ok.

%% @doc glPushName
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec popName() -> 'ok'.
popName()  ->
  IF = get_interface(),
  IF:queue_cmd(5314),
  ok.

%% @doc glBlendColor
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec blendColor(Red, Green, Blue, Alpha) -> 'ok' when Red :: clamp(),Green :: clamp(),Blue :: clamp(),Alpha :: clamp().
blendColor(Red,Green,Blue,Alpha) when is_float(Red),is_float(Green),is_float(Blue),is_float(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5315),
  ok.

%% @doc glBlendEquation
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec blendEquation(Mode) -> 'ok' when Mode :: enum().
blendEquation(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5316),
  ok.

%% @doc glDrawRangeElements
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec drawRangeElements(Mode, Start, End, Count, Type, Indices) -> 'ok' when Mode :: enum(),Start :: integer(),End :: integer(),Count :: integer(),Type :: enum(),Indices :: offset()|mem().
drawRangeElements(Mode,Start,End,Count,Type,Indices) when is_integer(Mode),is_integer(Start),is_integer(End),is_integer(Count),is_integer(Type),is_integer(Indices) orelse is_tuple(Indices) orelse is_binary(Indices) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Start,End,Count,Type,Indices,5317),
  ok.

%% @doc glTexImage3D
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texImage3D(Target, Level, InternalFormat, Width, Height, Depth, Border, Format, Type, Pixels) -> 'ok' when Target :: enum(),Level :: integer(),InternalFormat :: integer(),Width :: integer(),Height :: integer(),Depth :: integer(),Border :: integer(),Format :: enum(),Type :: enum(),Pixels :: offset()|mem().
texImage3D(Target,Level,InternalFormat,Width,Height,Depth,Border,Format,Type,Pixels) when is_integer(Target),is_integer(Level),is_integer(InternalFormat),is_integer(Width),is_integer(Height),is_integer(Depth),is_integer(Border),is_integer(Format),is_integer(Type),is_integer(Pixels) orelse is_tuple(Pixels) orelse is_binary(Pixels) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,InternalFormat,Width,Height,Depth,Border,Format,Type,Pixels,5319),
  ok.

%% @doc glTexSubImage
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texSubImage3D(Target, Level, Xoffset, Yoffset, Zoffset, Width, Height, Depth, Format, Type, Pixels) -> 'ok' when Target :: enum(),Level :: integer(),Xoffset :: integer(),Yoffset :: integer(),Zoffset :: integer(),Width :: integer(),Height :: integer(),Depth :: integer(),Format :: enum(),Type :: enum(),Pixels :: offset()|mem().
texSubImage3D(Target,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,Type,Pixels) when is_integer(Target),is_integer(Level),is_integer(Xoffset),is_integer(Yoffset),is_integer(Zoffset),is_integer(Width),is_integer(Height),is_integer(Depth),is_integer(Format),is_integer(Type),is_integer(Pixels) orelse is_tuple(Pixels) orelse is_binary(Pixels) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,Type,Pixels,5321),
  ok.

%% @doc glCopyTexSubImage3D
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec copyTexSubImage3D(Target, Level, Xoffset, Yoffset, Zoffset, X, Y, Width, Height) -> 'ok' when Target :: enum(),Level :: integer(),Xoffset :: integer(),Yoffset :: integer(),Zoffset :: integer(),X :: integer(),Y :: integer(),Width :: integer(),Height :: integer().
copyTexSubImage3D(Target,Level,Xoffset,Yoffset,Zoffset,X,Y,Width,Height) when is_integer(Target),is_integer(Level),is_integer(Xoffset),is_integer(Yoffset),is_integer(Zoffset),is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Xoffset,Yoffset,Zoffset,X,Y,Width,Height,5323),
  ok.

%% @doc glColorTable
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec colorTable(Target, Internalformat, Width, Format, Type, Table) -> 'ok' when Target :: enum(),Internalformat :: enum(),Width :: integer(),Format :: enum(),Type :: enum(),Table :: offset()|mem().
colorTable(Target,Internalformat,Width,Format,Type,Table) when is_integer(Target),is_integer(Internalformat),is_integer(Width),is_integer(Format),is_integer(Type),is_integer(Table) orelse is_tuple(Table) orelse is_binary(Table) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Width,Format,Type,Table,5324),
  ok.

%% @doc glColorTableParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec colorTableParameterfv(Target, Pname, Params) -> 'ok' when Target :: enum(),Pname :: enum(),Params :: {float(),float(),float(),float()}.
colorTableParameterfv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),tuple_size(Params) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5326),
  ok.

%% @doc glColorTableParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec colorTableParameteriv(Target, Pname, Params) -> 'ok' when Target :: enum(),Pname :: enum(),Params :: {integer(),integer(),integer(),integer()}.
colorTableParameteriv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),tuple_size(Params) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5327),
  ok.

%% @doc glCopyColorTable
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec copyColorTable(Target, Internalformat, X, Y, Width) -> 'ok' when Target :: enum(),Internalformat :: enum(),X :: integer(),Y :: integer(),Width :: integer().
copyColorTable(Target,Internalformat,X,Y,Width) when is_integer(Target),is_integer(Internalformat),is_integer(X),is_integer(Y),is_integer(Width) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,X,Y,Width,5328),
  ok.

%% @doc glGetColorTable
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getColorTable(Target, Format, Type, Table) -> 'ok' when Target :: enum(),Format :: enum(),Type :: enum(),Table :: mem().
getColorTable(Target,Format,Type,Table) when is_integer(Target),is_integer(Format),is_integer(Type),is_tuple(Table) orelse is_binary(Table) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Format,Type,Table,5329),
  rec(5329).

%% @doc glGetColorTableParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getColorTableParameterfv(Target, Pname) -> {float(),float(),float(),float()} when Target :: enum(),Pname :: enum().
getColorTableParameterfv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5330),
  rec(5330).

%% @doc glGetColorTableParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getColorTableParameteriv(Target, Pname) -> {integer(),integer(),integer(),integer()} when Target :: enum(),Pname :: enum().
getColorTableParameteriv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5331),
  rec(5331).

%% @doc glColorSubTable
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec colorSubTable(Target, Start, Count, Format, Type, Data) -> 'ok' when Target :: enum(),Start :: integer(),Count :: integer(),Format :: enum(),Type :: enum(),Data :: offset()|mem().
colorSubTable(Target,Start,Count,Format,Type,Data) when is_integer(Target),is_integer(Start),is_integer(Count),is_integer(Format),is_integer(Type),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Start,Count,Format,Type,Data,5332),
  ok.

%% @doc glCopyColorSubTable
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec copyColorSubTable(Target, Start, X, Y, Width) -> 'ok' when Target :: enum(),Start :: integer(),X :: integer(),Y :: integer(),Width :: integer().
copyColorSubTable(Target,Start,X,Y,Width) when is_integer(Target),is_integer(Start),is_integer(X),is_integer(Y),is_integer(Width) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Start,X,Y,Width,5334),
  ok.

%% @doc glConvolutionFilter1D
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec convolutionFilter1D(Target, Internalformat, Width, Format, Type, Image) -> 'ok' when Target :: enum(),Internalformat :: enum(),Width :: integer(),Format :: enum(),Type :: enum(),Image :: offset()|mem().
convolutionFilter1D(Target,Internalformat,Width,Format,Type,Image) when is_integer(Target),is_integer(Internalformat),is_integer(Width),is_integer(Format),is_integer(Type),is_integer(Image) orelse is_tuple(Image) orelse is_binary(Image) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Width,Format,Type,Image,5335),
  ok.

%% @doc glConvolutionFilter2D
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec convolutionFilter2D(Target, Internalformat, Width, Height, Format, Type, Image) -> 'ok' when Target :: enum(),Internalformat :: enum(),Width :: integer(),Height :: integer(),Format :: enum(),Type :: enum(),Image :: offset()|mem().
convolutionFilter2D(Target,Internalformat,Width,Height,Format,Type,Image) when is_integer(Target),is_integer(Internalformat),is_integer(Width),is_integer(Height),is_integer(Format),is_integer(Type),is_integer(Image) orelse is_tuple(Image) orelse is_binary(Image) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Width,Height,Format,Type,Image,5337),
  ok.

%% @doc glConvolutionParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec convolutionParameterf(Target, Pname, Params) -> 'ok' when Target :: enum(),Pname :: enum(),Params :: tuple().
convolutionParameterf(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5339),
  ok.

%% @doc glConvolutionParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec convolutionParameterfv(Target, Pname, Params) -> 'ok' when Target :: enum(),Pname :: enum(),Params :: tuple().
convolutionParameterfv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5340),
  ok.

%% @doc glConvolutionParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec convolutionParameteri(Target, Pname, Params) -> 'ok' when Target :: enum(),Pname :: enum(),Params :: tuple().
convolutionParameteri(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5341),
  ok.

%% @doc glConvolutionParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec convolutionParameteriv(Target, Pname, Params) -> 'ok' when Target :: enum(),Pname :: enum(),Params :: tuple().
convolutionParameteriv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5342),
  ok.

%% @doc glCopyConvolutionFilter1D
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec copyConvolutionFilter1D(Target, Internalformat, X, Y, Width) -> 'ok' when Target :: enum(),Internalformat :: enum(),X :: integer(),Y :: integer(),Width :: integer().
copyConvolutionFilter1D(Target,Internalformat,X,Y,Width) when is_integer(Target),is_integer(Internalformat),is_integer(X),is_integer(Y),is_integer(Width) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,X,Y,Width,5343),
  ok.

%% @doc glCopyConvolutionFilter2D
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec copyConvolutionFilter2D(Target, Internalformat, X, Y, Width, Height) -> 'ok' when Target :: enum(),Internalformat :: enum(),X :: integer(),Y :: integer(),Width :: integer(),Height :: integer().
copyConvolutionFilter2D(Target,Internalformat,X,Y,Width,Height) when is_integer(Target),is_integer(Internalformat),is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,X,Y,Width,Height,5344),
  ok.

%% @doc glGetConvolutionFilter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getConvolutionFilter(Target, Format, Type, Image) -> 'ok' when Target :: enum(),Format :: enum(),Type :: enum(),Image :: mem().
getConvolutionFilter(Target,Format,Type,Image) when is_integer(Target),is_integer(Format),is_integer(Type),is_tuple(Image) orelse is_binary(Image) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Format,Type,Image,5345),
  rec(5345).

%% @doc glGetConvolutionParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getConvolutionParameterfv(Target, Pname) -> {float(),float(),float(),float()} when Target :: enum(),Pname :: enum().
getConvolutionParameterfv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5346),
  rec(5346).

%% @doc glGetConvolutionParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getConvolutionParameteriv(Target, Pname) -> {integer(),integer(),integer(),integer()} when Target :: enum(),Pname :: enum().
getConvolutionParameteriv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5347),
  rec(5347).

%% @doc glSeparableFilter2D
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec separableFilter2D(Target, Internalformat, Width, Height, Format, Type, Row, Column) -> 'ok' when Target :: enum(),Internalformat :: enum(),Width :: integer(),Height :: integer(),Format :: enum(),Type :: enum(),Row :: offset()|mem(),Column :: offset()|mem().
separableFilter2D(Target,Internalformat,Width,Height,Format,Type,Row,Column) when is_integer(Target),is_integer(Internalformat),is_integer(Width),is_integer(Height),is_integer(Format),is_integer(Type),is_integer(Row) orelse is_tuple(Row) orelse is_binary(Row),is_integer(Column) orelse is_tuple(Column) orelse is_binary(Column) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Width,Height,Format,Type,Row,Column,5348),
  ok.

%% @doc glGetHistogram
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getHistogram(Target, Reset, Format, Type, Values) -> 'ok' when Target :: enum(),Reset :: 0|1,Format :: enum(),Type :: enum(),Values :: mem().
getHistogram(Target,Reset,Format,Type,Values) when is_integer(Target),(0 =:= Reset) orelse (1 =:= Reset),is_integer(Format),is_integer(Type),is_tuple(Values) orelse is_binary(Values) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Reset,Format,Type,Values,5350),
  rec(5350).

%% @doc glGetHistogramParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getHistogramParameterfv(Target, Pname) -> {float()} when Target :: enum(),Pname :: enum().
getHistogramParameterfv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5351),
  rec(5351).

%% @doc glGetHistogramParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getHistogramParameteriv(Target, Pname) -> {integer()} when Target :: enum(),Pname :: enum().
getHistogramParameteriv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5352),
  rec(5352).

%% @doc glGetMinmax
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getMinmax(Target, Reset, Format, Types, Values) -> 'ok' when Target :: enum(),Reset :: 0|1,Format :: enum(),Types :: enum(),Values :: mem().
getMinmax(Target,Reset,Format,Types,Values) when is_integer(Target),(0 =:= Reset) orelse (1 =:= Reset),is_integer(Format),is_integer(Types),is_tuple(Values) orelse is_binary(Values) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Reset,Format,Types,Values,5353),
  rec(5353).

%% @doc glGetMinmaxParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getMinmaxParameterfv(Target, Pname) -> {float()} when Target :: enum(),Pname :: enum().
getMinmaxParameterfv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5354),
  rec(5354).

%% @doc glGetMinmaxParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getMinmaxParameteriv(Target, Pname) -> {integer()} when Target :: enum(),Pname :: enum().
getMinmaxParameteriv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5355),
  rec(5355).

%% @doc glHistogram
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec histogram(Target, Width, Internalformat, Sink) -> 'ok' when Target :: enum(),Width :: integer(),Internalformat :: enum(),Sink :: 0|1.
histogram(Target,Width,Internalformat,Sink) when is_integer(Target),is_integer(Width),is_integer(Internalformat),(0 =:= Sink) orelse (1 =:= Sink) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Width,Internalformat,Sink,5356),
  ok.

%% @doc glMinmax
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec minmax(Target, Internalformat, Sink) -> 'ok' when Target :: enum(),Internalformat :: enum(),Sink :: 0|1.
minmax(Target,Internalformat,Sink) when is_integer(Target),is_integer(Internalformat),(0 =:= Sink) orelse (1 =:= Sink) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Sink,5357),
  ok.

%% @doc glResetHistogram
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec resetHistogram(Target) -> 'ok' when Target :: enum().
resetHistogram(Target) when is_integer(Target) ->
  IF = get_interface(),
  IF:queue_cmd(Target,5358),
  ok.

%% @doc glResetMinmax
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec resetMinmax(Target) -> 'ok' when Target :: enum().
resetMinmax(Target) when is_integer(Target) ->
  IF = get_interface(),
  IF:queue_cmd(Target,5359),
  ok.

%% @doc glActiveTexture
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec activeTexture(Texture) -> 'ok' when Texture :: enum().
activeTexture(Texture) when is_integer(Texture) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,5360),
  ok.

%% @doc glSampleCoverage
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec sampleCoverage(Value, Invert) -> 'ok' when Value :: clamp(),Invert :: 0|1.
sampleCoverage(Value,Invert) when is_float(Value),(0 =:= Invert) orelse (1 =:= Invert) ->
  IF = get_interface(),
  IF:queue_cmd(Value,Invert,5361),
  ok.

%% @doc glCompressedTexImage3D
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec compressedTexImage3D(Target, Level, Internalformat, Width, Height, Depth, Border, ImageSize, Data) -> 'ok' when Target :: enum(),Level :: integer(),Internalformat :: enum(),Width :: integer(),Height :: integer(),Depth :: integer(),Border :: integer(),ImageSize :: integer(),Data :: offset()|mem().
compressedTexImage3D(Target,Level,Internalformat,Width,Height,Depth,Border,ImageSize,Data) when is_integer(Target),is_integer(Level),is_integer(Internalformat),is_integer(Width),is_integer(Height),is_integer(Depth),is_integer(Border),is_integer(ImageSize),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Internalformat,Width,Height,Depth,Border,ImageSize,Data,5362),
  ok.

%% @doc glCompressedTexImage2D
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec compressedTexImage2D(Target, Level, Internalformat, Width, Height, Border, ImageSize, Data) -> 'ok' when Target :: enum(),Level :: integer(),Internalformat :: enum(),Width :: integer(),Height :: integer(),Border :: integer(),ImageSize :: integer(),Data :: offset()|mem().
compressedTexImage2D(Target,Level,Internalformat,Width,Height,Border,ImageSize,Data) when is_integer(Target),is_integer(Level),is_integer(Internalformat),is_integer(Width),is_integer(Height),is_integer(Border),is_integer(ImageSize),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Internalformat,Width,Height,Border,ImageSize,Data,5364),
  ok.

%% @doc glCompressedTexImage1D
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec compressedTexImage1D(Target, Level, Internalformat, Width, Border, ImageSize, Data) -> 'ok' when Target :: enum(),Level :: integer(),Internalformat :: enum(),Width :: integer(),Border :: integer(),ImageSize :: integer(),Data :: offset()|mem().
compressedTexImage1D(Target,Level,Internalformat,Width,Border,ImageSize,Data) when is_integer(Target),is_integer(Level),is_integer(Internalformat),is_integer(Width),is_integer(Border),is_integer(ImageSize),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Internalformat,Width,Border,ImageSize,Data,5366),
  ok.

%% @doc glCompressedTexSubImage3D
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec compressedTexSubImage3D(Target, Level, Xoffset, Yoffset, Zoffset, Width, Height, Depth, Format, ImageSize, Data) -> 'ok' when Target :: enum(),Level :: integer(),Xoffset :: integer(),Yoffset :: integer(),Zoffset :: integer(),Width :: integer(),Height :: integer(),Depth :: integer(),Format :: enum(),ImageSize :: integer(),Data :: offset()|mem().
compressedTexSubImage3D(Target,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,ImageSize,Data) when is_integer(Target),is_integer(Level),is_integer(Xoffset),is_integer(Yoffset),is_integer(Zoffset),is_integer(Width),is_integer(Height),is_integer(Depth),is_integer(Format),is_integer(ImageSize),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,ImageSize,Data,5368),
  ok.

%% @doc glCompressedTexSubImage2D
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec compressedTexSubImage2D(Target, Level, Xoffset, Yoffset, Width, Height, Format, ImageSize, Data) -> 'ok' when Target :: enum(),Level :: integer(),Xoffset :: integer(),Yoffset :: integer(),Width :: integer(),Height :: integer(),Format :: enum(),ImageSize :: integer(),Data :: offset()|mem().
compressedTexSubImage2D(Target,Level,Xoffset,Yoffset,Width,Height,Format,ImageSize,Data) when is_integer(Target),is_integer(Level),is_integer(Xoffset),is_integer(Yoffset),is_integer(Width),is_integer(Height),is_integer(Format),is_integer(ImageSize),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Xoffset,Yoffset,Width,Height,Format,ImageSize,Data,5370),
  ok.

%% @doc glCompressedTexSubImage1D
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec compressedTexSubImage1D(Target, Level, Xoffset, Width, Format, ImageSize, Data) -> 'ok' when Target :: enum(),Level :: integer(),Xoffset :: integer(),Width :: integer(),Format :: enum(),ImageSize :: integer(),Data :: offset()|mem().
compressedTexSubImage1D(Target,Level,Xoffset,Width,Format,ImageSize,Data) when is_integer(Target),is_integer(Level),is_integer(Xoffset),is_integer(Width),is_integer(Format),is_integer(ImageSize),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Xoffset,Width,Format,ImageSize,Data,5372),
  ok.

%% @doc glGetCompressedTexImage
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getCompressedTexImage(Target, Lod, Img) -> 'ok' when Target :: enum(),Lod :: integer(),Img :: mem().
getCompressedTexImage(Target,Lod,Img) when is_integer(Target),is_integer(Lod),is_tuple(Img) orelse is_binary(Img) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Lod,Img,5374),
  rec(5374).

%% @doc glClientActiveTexture
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec clientActiveTexture(Texture) -> 'ok' when Texture :: enum().
clientActiveTexture(Texture) when is_integer(Texture) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,5375),
  ok.

%% @doc glMultiTexCoord
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec multiTexCoord1d(Target, S) -> 'ok' when Target :: enum(),S :: float().
multiTexCoord1d(Target,S) when is_integer(Target),is_float(S) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,5376),
  ok.

%% @equiv multiTexCoord1d(Target,S)
-spec multiTexCoord1dv(Target :: enum(),V) -> 'ok' when V :: {S :: float()}.
multiTexCoord1dv(Target,{S}) ->  multiTexCoord1d(Target,S).
%% @doc glMultiTexCoord
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec multiTexCoord1f(Target, S) -> 'ok' when Target :: enum(),S :: float().
multiTexCoord1f(Target,S) when is_integer(Target),is_float(S) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,5377),
  ok.

%% @equiv multiTexCoord1f(Target,S)
-spec multiTexCoord1fv(Target :: enum(),V) -> 'ok' when V :: {S :: float()}.
multiTexCoord1fv(Target,{S}) ->  multiTexCoord1f(Target,S).
%% @doc glMultiTexCoord
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec multiTexCoord1i(Target, S) -> 'ok' when Target :: enum(),S :: integer().
multiTexCoord1i(Target,S) when is_integer(Target),is_integer(S) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,5378),
  ok.

%% @equiv multiTexCoord1i(Target,S)
-spec multiTexCoord1iv(Target :: enum(),V) -> 'ok' when V :: {S :: integer()}.
multiTexCoord1iv(Target,{S}) ->  multiTexCoord1i(Target,S).
%% @doc glMultiTexCoord
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec multiTexCoord1s(Target, S) -> 'ok' when Target :: enum(),S :: integer().
multiTexCoord1s(Target,S) when is_integer(Target),is_integer(S) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,5379),
  ok.

%% @equiv multiTexCoord1s(Target,S)
-spec multiTexCoord1sv(Target :: enum(),V) -> 'ok' when V :: {S :: integer()}.
multiTexCoord1sv(Target,{S}) ->  multiTexCoord1s(Target,S).
%% @doc glMultiTexCoord
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec multiTexCoord2d(Target, S, T) -> 'ok' when Target :: enum(),S :: float(),T :: float().
multiTexCoord2d(Target,S,T) when is_integer(Target),is_float(S),is_float(T) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,5380),
  ok.

%% @equiv multiTexCoord2d(Target,S,T)
-spec multiTexCoord2dv(Target :: enum(),V) -> 'ok' when V :: {S :: float(),T :: float()}.
multiTexCoord2dv(Target,{S,T}) ->  multiTexCoord2d(Target,S,T).
%% @doc glMultiTexCoord
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec multiTexCoord2f(Target, S, T) -> 'ok' when Target :: enum(),S :: float(),T :: float().
multiTexCoord2f(Target,S,T) when is_integer(Target),is_float(S),is_float(T) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,5381),
  ok.

%% @equiv multiTexCoord2f(Target,S,T)
-spec multiTexCoord2fv(Target :: enum(),V) -> 'ok' when V :: {S :: float(),T :: float()}.
multiTexCoord2fv(Target,{S,T}) ->  multiTexCoord2f(Target,S,T).
%% @doc glMultiTexCoord
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec multiTexCoord2i(Target, S, T) -> 'ok' when Target :: enum(),S :: integer(),T :: integer().
multiTexCoord2i(Target,S,T) when is_integer(Target),is_integer(S),is_integer(T) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,5382),
  ok.

%% @equiv multiTexCoord2i(Target,S,T)
-spec multiTexCoord2iv(Target :: enum(),V) -> 'ok' when V :: {S :: integer(),T :: integer()}.
multiTexCoord2iv(Target,{S,T}) ->  multiTexCoord2i(Target,S,T).
%% @doc glMultiTexCoord
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec multiTexCoord2s(Target, S, T) -> 'ok' when Target :: enum(),S :: integer(),T :: integer().
multiTexCoord2s(Target,S,T) when is_integer(Target),is_integer(S),is_integer(T) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,5383),
  ok.

%% @equiv multiTexCoord2s(Target,S,T)
-spec multiTexCoord2sv(Target :: enum(),V) -> 'ok' when V :: {S :: integer(),T :: integer()}.
multiTexCoord2sv(Target,{S,T}) ->  multiTexCoord2s(Target,S,T).
%% @doc glMultiTexCoord
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec multiTexCoord3d(Target, S, T, R) -> 'ok' when Target :: enum(),S :: float(),T :: float(),R :: float().
multiTexCoord3d(Target,S,T,R) when is_integer(Target),is_float(S),is_float(T),is_float(R) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,R,5384),
  ok.

%% @equiv multiTexCoord3d(Target,S,T,R)
-spec multiTexCoord3dv(Target :: enum(),V) -> 'ok' when V :: {S :: float(),T :: float(),R :: float()}.
multiTexCoord3dv(Target,{S,T,R}) ->  multiTexCoord3d(Target,S,T,R).
%% @doc glMultiTexCoord
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec multiTexCoord3f(Target, S, T, R) -> 'ok' when Target :: enum(),S :: float(),T :: float(),R :: float().
multiTexCoord3f(Target,S,T,R) when is_integer(Target),is_float(S),is_float(T),is_float(R) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,R,5385),
  ok.

%% @equiv multiTexCoord3f(Target,S,T,R)
-spec multiTexCoord3fv(Target :: enum(),V) -> 'ok' when V :: {S :: float(),T :: float(),R :: float()}.
multiTexCoord3fv(Target,{S,T,R}) ->  multiTexCoord3f(Target,S,T,R).
%% @doc glMultiTexCoord
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec multiTexCoord3i(Target, S, T, R) -> 'ok' when Target :: enum(),S :: integer(),T :: integer(),R :: integer().
multiTexCoord3i(Target,S,T,R) when is_integer(Target),is_integer(S),is_integer(T),is_integer(R) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,R,5386),
  ok.

%% @equiv multiTexCoord3i(Target,S,T,R)
-spec multiTexCoord3iv(Target :: enum(),V) -> 'ok' when V :: {S :: integer(),T :: integer(),R :: integer()}.
multiTexCoord3iv(Target,{S,T,R}) ->  multiTexCoord3i(Target,S,T,R).
%% @doc glMultiTexCoord
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec multiTexCoord3s(Target, S, T, R) -> 'ok' when Target :: enum(),S :: integer(),T :: integer(),R :: integer().
multiTexCoord3s(Target,S,T,R) when is_integer(Target),is_integer(S),is_integer(T),is_integer(R) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,R,5387),
  ok.

%% @equiv multiTexCoord3s(Target,S,T,R)
-spec multiTexCoord3sv(Target :: enum(),V) -> 'ok' when V :: {S :: integer(),T :: integer(),R :: integer()}.
multiTexCoord3sv(Target,{S,T,R}) ->  multiTexCoord3s(Target,S,T,R).
%% @doc glMultiTexCoord
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec multiTexCoord4d(Target, S, T, R, Q) -> 'ok' when Target :: enum(),S :: float(),T :: float(),R :: float(),Q :: float().
multiTexCoord4d(Target,S,T,R,Q) when is_integer(Target),is_float(S),is_float(T),is_float(R),is_float(Q) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,R,Q,5388),
  ok.

%% @equiv multiTexCoord4d(Target,S,T,R,Q)
-spec multiTexCoord4dv(Target :: enum(),V) -> 'ok' when V :: {S :: float(),T :: float(),R :: float(),Q :: float()}.
multiTexCoord4dv(Target,{S,T,R,Q}) ->  multiTexCoord4d(Target,S,T,R,Q).
%% @doc glMultiTexCoord
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec multiTexCoord4f(Target, S, T, R, Q) -> 'ok' when Target :: enum(),S :: float(),T :: float(),R :: float(),Q :: float().
multiTexCoord4f(Target,S,T,R,Q) when is_integer(Target),is_float(S),is_float(T),is_float(R),is_float(Q) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,R,Q,5389),
  ok.

%% @equiv multiTexCoord4f(Target,S,T,R,Q)
-spec multiTexCoord4fv(Target :: enum(),V) -> 'ok' when V :: {S :: float(),T :: float(),R :: float(),Q :: float()}.
multiTexCoord4fv(Target,{S,T,R,Q}) ->  multiTexCoord4f(Target,S,T,R,Q).
%% @doc glMultiTexCoord
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec multiTexCoord4i(Target, S, T, R, Q) -> 'ok' when Target :: enum(),S :: integer(),T :: integer(),R :: integer(),Q :: integer().
multiTexCoord4i(Target,S,T,R,Q) when is_integer(Target),is_integer(S),is_integer(T),is_integer(R),is_integer(Q) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,R,Q,5390),
  ok.

%% @equiv multiTexCoord4i(Target,S,T,R,Q)
-spec multiTexCoord4iv(Target :: enum(),V) -> 'ok' when V :: {S :: integer(),T :: integer(),R :: integer(),Q :: integer()}.
multiTexCoord4iv(Target,{S,T,R,Q}) ->  multiTexCoord4i(Target,S,T,R,Q).
%% @doc glMultiTexCoord
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec multiTexCoord4s(Target, S, T, R, Q) -> 'ok' when Target :: enum(),S :: integer(),T :: integer(),R :: integer(),Q :: integer().
multiTexCoord4s(Target,S,T,R,Q) when is_integer(Target),is_integer(S),is_integer(T),is_integer(R),is_integer(Q) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,R,Q,5391),
  ok.

%% @equiv multiTexCoord4s(Target,S,T,R,Q)
-spec multiTexCoord4sv(Target :: enum(),V) -> 'ok' when V :: {S :: integer(),T :: integer(),R :: integer(),Q :: integer()}.
multiTexCoord4sv(Target,{S,T,R,Q}) ->  multiTexCoord4s(Target,S,T,R,Q).
%% @doc glLoadTransposeMatrix
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec loadTransposeMatrixf(M) -> 'ok' when M :: matrix().
loadTransposeMatrixf(M) when tuple_size(M) =:= 16 ->
  IF = get_interface(),
  IF:queue_cmd(M,5392),
  ok.

%% @doc glLoadTransposeMatrix
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec loadTransposeMatrixd(M) -> 'ok' when M :: matrix().
loadTransposeMatrixd(M) when tuple_size(M) =:= 16 ->
  IF = get_interface(),
  IF:queue_cmd(M,5393),
  ok.

%% @doc glMultTransposeMatrix
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec multTransposeMatrixf(M) -> 'ok' when M :: matrix().
multTransposeMatrixf(M) when tuple_size(M) =:= 16 ->
  IF = get_interface(),
  IF:queue_cmd(M,5394),
  ok.

%% @doc glMultTransposeMatrix
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec multTransposeMatrixd(M) -> 'ok' when M :: matrix().
multTransposeMatrixd(M) when tuple_size(M) =:= 16 ->
  IF = get_interface(),
  IF:queue_cmd(M,5395),
  ok.

%% @doc glBlendFuncSeparate
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec blendFuncSeparate(SfactorRGB, DfactorRGB, SfactorAlpha, DfactorAlpha) -> 'ok' when SfactorRGB :: enum(),DfactorRGB :: enum(),SfactorAlpha :: enum(),DfactorAlpha :: enum().
blendFuncSeparate(SfactorRGB,DfactorRGB,SfactorAlpha,DfactorAlpha) when is_integer(SfactorRGB),is_integer(DfactorRGB),is_integer(SfactorAlpha),is_integer(DfactorAlpha) ->
  IF = get_interface(),
  IF:queue_cmd(SfactorRGB,DfactorRGB,SfactorAlpha,DfactorAlpha,5396),
  ok.

%% @doc glMultiDrawArrays
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec multiDrawArrays(Mode, First, Count) -> 'ok' when Mode :: enum(),First :: [integer()]|mem(),Count :: [integer()]|mem().
multiDrawArrays(Mode,First,Count) when is_integer(Mode),is_list(First) orelse is_tuple(First) orelse is_binary(First),is_list(Count) orelse is_tuple(Count) orelse is_binary(Count) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,First,Count,5397),
  ok.

%% @doc glPointParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec pointParameterf(Pname, Param) -> 'ok' when Pname :: enum(),Param :: float().
pointParameterf(Pname,Param) when is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5399),
  ok.

%% @doc glPointParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec pointParameterfv(Pname, Params) -> 'ok' when Pname :: enum(),Params :: tuple().
pointParameterfv(Pname,Params) when is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Params,5400),
  ok.

%% @doc glPointParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec pointParameteri(Pname, Param) -> 'ok' when Pname :: enum(),Param :: integer().
pointParameteri(Pname,Param) when is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5401),
  ok.

%% @doc glPointParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec pointParameteriv(Pname, Params) -> 'ok' when Pname :: enum(),Params :: tuple().
pointParameteriv(Pname,Params) when is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Params,5402),
  ok.

%% @doc glFogCoord
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec fogCoordf(Coord) -> 'ok' when Coord :: float().
fogCoordf(Coord) when is_float(Coord) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,5403),
  ok.

%% @equiv fogCoordf(Coord)
-spec fogCoordfv(Coord) -> 'ok' when Coord :: {Coord :: float()}.
fogCoordfv({Coord}) ->  fogCoordf(Coord).
%% @doc glFogCoord
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec fogCoordd(Coord) -> 'ok' when Coord :: float().
fogCoordd(Coord) when is_float(Coord) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,5404),
  ok.

%% @equiv fogCoordd(Coord)
-spec fogCoorddv(Coord) -> 'ok' when Coord :: {Coord :: float()}.
fogCoorddv({Coord}) ->  fogCoordd(Coord).
%% @doc glFogCoordPointer
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec fogCoordPointer(Type, Stride, Pointer) -> 'ok' when Type :: enum(),Stride :: integer(),Pointer :: offset()|mem().
fogCoordPointer(Type,Stride,Pointer) when is_integer(Type),is_integer(Stride),is_integer(Pointer) orelse is_tuple(Pointer) orelse is_binary(Pointer) ->
  IF = get_interface(),
  IF:queue_cmd(Type,Stride,Pointer,5405),
  ok.

%% @doc glSecondaryColor
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec secondaryColor3b(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
secondaryColor3b(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5407),
  ok.

%% @equiv secondaryColor3b(Red,Green,Blue)
-spec secondaryColor3bv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer()}.
secondaryColor3bv({Red,Green,Blue}) ->  secondaryColor3b(Red,Green,Blue).
%% @doc glSecondaryColor
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec secondaryColor3d(Red, Green, Blue) -> 'ok' when Red :: float(),Green :: float(),Blue :: float().
secondaryColor3d(Red,Green,Blue) when is_float(Red),is_float(Green),is_float(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5408),
  ok.

%% @equiv secondaryColor3d(Red,Green,Blue)
-spec secondaryColor3dv(V) -> 'ok' when V :: {Red :: float(),Green :: float(),Blue :: float()}.
secondaryColor3dv({Red,Green,Blue}) ->  secondaryColor3d(Red,Green,Blue).
%% @doc glSecondaryColor
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec secondaryColor3f(Red, Green, Blue) -> 'ok' when Red :: float(),Green :: float(),Blue :: float().
secondaryColor3f(Red,Green,Blue) when is_float(Red),is_float(Green),is_float(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5409),
  ok.

%% @equiv secondaryColor3f(Red,Green,Blue)
-spec secondaryColor3fv(V) -> 'ok' when V :: {Red :: float(),Green :: float(),Blue :: float()}.
secondaryColor3fv({Red,Green,Blue}) ->  secondaryColor3f(Red,Green,Blue).
%% @doc glSecondaryColor
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec secondaryColor3i(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
secondaryColor3i(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5410),
  ok.

%% @equiv secondaryColor3i(Red,Green,Blue)
-spec secondaryColor3iv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer()}.
secondaryColor3iv({Red,Green,Blue}) ->  secondaryColor3i(Red,Green,Blue).
%% @doc glSecondaryColor
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec secondaryColor3s(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
secondaryColor3s(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5411),
  ok.

%% @equiv secondaryColor3s(Red,Green,Blue)
-spec secondaryColor3sv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer()}.
secondaryColor3sv({Red,Green,Blue}) ->  secondaryColor3s(Red,Green,Blue).
%% @doc glSecondaryColor
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec secondaryColor3ub(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
secondaryColor3ub(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5412),
  ok.

%% @equiv secondaryColor3ub(Red,Green,Blue)
-spec secondaryColor3ubv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer()}.
secondaryColor3ubv({Red,Green,Blue}) ->  secondaryColor3ub(Red,Green,Blue).
%% @doc glSecondaryColor
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec secondaryColor3ui(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
secondaryColor3ui(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5413),
  ok.

%% @equiv secondaryColor3ui(Red,Green,Blue)
-spec secondaryColor3uiv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer()}.
secondaryColor3uiv({Red,Green,Blue}) ->  secondaryColor3ui(Red,Green,Blue).
%% @doc glSecondaryColor
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec secondaryColor3us(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
secondaryColor3us(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5414),
  ok.

%% @equiv secondaryColor3us(Red,Green,Blue)
-spec secondaryColor3usv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer()}.
secondaryColor3usv({Red,Green,Blue}) ->  secondaryColor3us(Red,Green,Blue).
%% @doc glSecondaryColorPointer
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec secondaryColorPointer(Size, Type, Stride, Pointer) -> 'ok' when Size :: integer(),Type :: enum(),Stride :: integer(),Pointer :: offset()|mem().
secondaryColorPointer(Size,Type,Stride,Pointer) when is_integer(Size),is_integer(Type),is_integer(Stride),is_integer(Pointer) orelse is_tuple(Pointer) orelse is_binary(Pointer) ->
  IF = get_interface(),
  IF:queue_cmd(Size,Type,Stride,Pointer,5415),
  ok.

%% @doc glWindowPos
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec windowPos2d(X, Y) -> 'ok' when X :: float(),Y :: float().
windowPos2d(X,Y) when is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5417),
  ok.

%% @equiv windowPos2d(X,Y)
-spec windowPos2dv(V) -> 'ok' when V :: {X :: float(),Y :: float()}.
windowPos2dv({X,Y}) ->  windowPos2d(X,Y).
%% @doc glWindowPos
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec windowPos2f(X, Y) -> 'ok' when X :: float(),Y :: float().
windowPos2f(X,Y) when is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5418),
  ok.

%% @equiv windowPos2f(X,Y)
-spec windowPos2fv(V) -> 'ok' when V :: {X :: float(),Y :: float()}.
windowPos2fv({X,Y}) ->  windowPos2f(X,Y).
%% @doc glWindowPos
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec windowPos2i(X, Y) -> 'ok' when X :: integer(),Y :: integer().
windowPos2i(X,Y) when is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5419),
  ok.

%% @equiv windowPos2i(X,Y)
-spec windowPos2iv(V) -> 'ok' when V :: {X :: integer(),Y :: integer()}.
windowPos2iv({X,Y}) ->  windowPos2i(X,Y).
%% @doc glWindowPos
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec windowPos2s(X, Y) -> 'ok' when X :: integer(),Y :: integer().
windowPos2s(X,Y) when is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5420),
  ok.

%% @equiv windowPos2s(X,Y)
-spec windowPos2sv(V) -> 'ok' when V :: {X :: integer(),Y :: integer()}.
windowPos2sv({X,Y}) ->  windowPos2s(X,Y).
%% @doc glWindowPos
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec windowPos3d(X, Y, Z) -> 'ok' when X :: float(),Y :: float(),Z :: float().
windowPos3d(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5421),
  ok.

%% @equiv windowPos3d(X,Y,Z)
-spec windowPos3dv(V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float()}.
windowPos3dv({X,Y,Z}) ->  windowPos3d(X,Y,Z).
%% @doc glWindowPos
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec windowPos3f(X, Y, Z) -> 'ok' when X :: float(),Y :: float(),Z :: float().
windowPos3f(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5422),
  ok.

%% @equiv windowPos3f(X,Y,Z)
-spec windowPos3fv(V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float()}.
windowPos3fv({X,Y,Z}) ->  windowPos3f(X,Y,Z).
%% @doc glWindowPos
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec windowPos3i(X, Y, Z) -> 'ok' when X :: integer(),Y :: integer(),Z :: integer().
windowPos3i(X,Y,Z) when is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5423),
  ok.

%% @equiv windowPos3i(X,Y,Z)
-spec windowPos3iv(V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer()}.
windowPos3iv({X,Y,Z}) ->  windowPos3i(X,Y,Z).
%% @doc glWindowPos
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec windowPos3s(X, Y, Z) -> 'ok' when X :: integer(),Y :: integer(),Z :: integer().
windowPos3s(X,Y,Z) when is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5424),
  ok.

%% @equiv windowPos3s(X,Y,Z)
-spec windowPos3sv(V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer()}.
windowPos3sv({X,Y,Z}) ->  windowPos3s(X,Y,Z).
%% @doc glGenQueries
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec genQueries(N) -> [integer()] when N :: integer().
genQueries(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5425),
  rec(5425).

%% @doc glDeleteQueries
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec deleteQueries(Ids) -> 'ok' when Ids :: [integer()].
deleteQueries(Ids) when is_list(Ids) ->
  IF = get_interface(),
  N = length(Ids),
  IF:queue_cmd(N,Ids,5426),
  ok.

%% @doc glIsQuery
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec isQuery(Id) -> 0|1 when Id :: integer().
isQuery(Id) when is_integer(Id) ->
  IF = get_interface(),
  IF:queue_cmd(Id,5427),
  rec(5427).

%% @doc glBeginQuery
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec beginQuery(Target, Id) -> 'ok' when Target :: enum(),Id :: integer().
beginQuery(Target,Id) when is_integer(Target),is_integer(Id) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Id,5428),
  ok.

%% @doc glBeginQuery
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec endQuery(Target) -> 'ok' when Target :: enum().
endQuery(Target) when is_integer(Target) ->
  IF = get_interface(),
  IF:queue_cmd(Target,5429),
  ok.

%% @doc glGetQuery
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getQueryiv(Target, Pname) -> integer() when Target :: enum(),Pname :: enum().
getQueryiv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5430),
  rec(5430).

%% @doc glGetQueryObject
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getQueryObjectiv(Id, Pname) -> integer() when Id :: integer(),Pname :: enum().
getQueryObjectiv(Id,Pname) when is_integer(Id),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Id,Pname,5431),
  rec(5431).

%% @doc glGetQueryObject
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getQueryObjectuiv(Id, Pname) -> integer() when Id :: integer(),Pname :: enum().
getQueryObjectuiv(Id,Pname) when is_integer(Id),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Id,Pname,5432),
  rec(5432).

%% @doc glBindBuffer
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec bindBuffer(Target, Buffer) -> 'ok' when Target :: enum(),Buffer :: integer().
bindBuffer(Target,Buffer) when is_integer(Target),is_integer(Buffer) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Buffer,5433),
  ok.

%% @doc glDeleteBuffers
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec deleteBuffers(Buffers) -> 'ok' when Buffers :: [integer()].
deleteBuffers(Buffers) when is_list(Buffers) ->
  IF = get_interface(),
  N = length(Buffers),
  IF:queue_cmd(N,Buffers,5434),
  ok.

%% @doc glGenBuffers
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec genBuffers(N) -> [integer()] when N :: integer().
genBuffers(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5435),
  rec(5435).

%% @doc glIsBuffer
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec isBuffer(Buffer) -> 0|1 when Buffer :: integer().
isBuffer(Buffer) when is_integer(Buffer) ->
  IF = get_interface(),
  IF:queue_cmd(Buffer,5436),
  rec(5436).

%% @doc glBufferData
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec bufferData(Target, Size, Data, Usage) -> 'ok' when Target :: enum(),Size :: integer(),Data :: offset()|mem(),Usage :: enum().
bufferData(Target,Size,Data,Usage) when is_integer(Target),is_integer(Size),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data),is_integer(Usage) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Size,Data,Usage,5437),
  ok.

%% @doc glBufferSubData
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec bufferSubData(Target, Offset, Size, Data) -> 'ok' when Target :: enum(),Offset :: integer(),Size :: integer(),Data :: offset()|mem().
bufferSubData(Target,Offset,Size,Data) when is_integer(Target),is_integer(Offset),is_integer(Size),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Offset,Size,Data,5439),
  ok.

%% @doc glGetBufferSubData
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getBufferSubData(Target, Offset, Size, Data) -> 'ok' when Target :: enum(),Offset :: integer(),Size :: integer(),Data :: mem().
getBufferSubData(Target,Offset,Size,Data) when is_integer(Target),is_integer(Offset),is_integer(Size),is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Offset,Size,Data,5441),
  rec(5441).

%% @doc glGetBufferParameteriv
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getBufferParameteriv(Target, Pname) -> integer() when Target :: enum(),Pname :: enum().
getBufferParameteriv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5442),
  rec(5442).

%% @doc glBlendEquationSeparate
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec blendEquationSeparate(ModeRGB, ModeAlpha) -> 'ok' when ModeRGB :: enum(),ModeAlpha :: enum().
blendEquationSeparate(ModeRGB,ModeAlpha) when is_integer(ModeRGB),is_integer(ModeAlpha) ->
  IF = get_interface(),
  IF:queue_cmd(ModeRGB,ModeAlpha,5443),
  ok.

%% @doc glDrawBuffers
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec drawBuffers(Bufs) -> 'ok' when Bufs :: [enum()].
drawBuffers(Bufs) when is_list(Bufs) ->
  IF = get_interface(),
  N = length(Bufs),
  IF:queue_cmd(N,Bufs,5444),
  ok.

%% @doc glStencilOpSeparate
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec stencilOpSeparate(Face, Sfail, Dpfail, Dppass) -> 'ok' when Face :: enum(),Sfail :: enum(),Dpfail :: enum(),Dppass :: enum().
stencilOpSeparate(Face,Sfail,Dpfail,Dppass) when is_integer(Face),is_integer(Sfail),is_integer(Dpfail),is_integer(Dppass) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Sfail,Dpfail,Dppass,5445),
  ok.

%% @doc glStencilFuncSeparate
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec stencilFuncSeparate(Face, Func, Ref, Mask) -> 'ok' when Face :: enum(),Func :: enum(),Ref :: integer(),Mask :: integer().
stencilFuncSeparate(Face,Func,Ref,Mask) when is_integer(Face),is_integer(Func),is_integer(Ref),is_integer(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Func,Ref,Mask,5446),
  ok.

%% @doc glStencilMaskSeparate
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec stencilMaskSeparate(Face, Mask) -> 'ok' when Face :: enum(),Mask :: integer().
stencilMaskSeparate(Face,Mask) when is_integer(Face),is_integer(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Mask,5447),
  ok.

%% @doc glAttachShader
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec attachShader(Program, Shader) -> 'ok' when Program :: integer(),Shader :: integer().
attachShader(Program,Shader) when is_integer(Program),is_integer(Shader) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Shader,5448),
  ok.

%% @doc glBindAttribLocation
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec bindAttribLocation(Program, Index, Name) -> 'ok' when Program :: integer(),Index :: integer(),Name :: string().
bindAttribLocation(Program,Index,Name) when is_integer(Program),is_integer(Index),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,Index,NameBin,5449),
  ok.

%% @doc glCompileShader
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec compileShader(Shader) -> 'ok' when Shader :: integer().
compileShader(Shader) when is_integer(Shader) ->
  IF = get_interface(),
  IF:queue_cmd(Shader,5450),
  ok.

%% @doc glCreateProgram
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec createProgram() -> integer().
createProgram()  ->
  IF = get_interface(),
  IF:queue_cmd(5451),
  rec(5451).

%% @doc glCreateShader
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec createShader(Type) -> integer() when Type :: enum().
createShader(Type) when is_integer(Type) ->
  IF = get_interface(),
  IF:queue_cmd(Type,5452),
  rec(5452).

%% @doc glDeleteProgram
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec deleteProgram(Program) -> 'ok' when Program :: integer().
deleteProgram(Program) when is_integer(Program) ->
  IF = get_interface(),
  IF:queue_cmd(Program,5453),
  ok.

%% @doc glDeleteShader
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec deleteShader(Shader) -> 'ok' when Shader :: integer().
deleteShader(Shader) when is_integer(Shader) ->
  IF = get_interface(),
  IF:queue_cmd(Shader,5454),
  ok.

%% @doc glDetachShader
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec detachShader(Program, Shader) -> 'ok' when Program :: integer(),Shader :: integer().
detachShader(Program,Shader) when is_integer(Program),is_integer(Shader) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Shader,5455),
  ok.

%% @doc glEnableVertexAttribArray
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec disableVertexAttribArray(Index) -> 'ok' when Index :: integer().
disableVertexAttribArray(Index) when is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Index,5456),
  ok.

%% @doc glEnableVertexAttribArray
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec enableVertexAttribArray(Index) -> 'ok' when Index :: integer().
enableVertexAttribArray(Index) when is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Index,5457),
  ok.

%% @doc glGetActiveAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getActiveAttrib(Program, Index, BufSize) -> {Size :: integer(),Type :: enum(),Name :: string()} when Program :: integer(),Index :: integer(),BufSize :: integer().
getActiveAttrib(Program,Index,BufSize) when is_integer(Program),is_integer(Index),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Index,BufSize,5458),
  rec(5458).

%% @doc glGetActiveUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getActiveUniform(Program, Index, BufSize) -> {Size :: integer(),Type :: enum(),Name :: string()} when Program :: integer(),Index :: integer(),BufSize :: integer().
getActiveUniform(Program,Index,BufSize) when is_integer(Program),is_integer(Index),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Index,BufSize,5459),
  rec(5459).

%% @doc glGetAttachedShaders
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getAttachedShaders(Program, MaxCount) -> [integer()] when Program :: integer(),MaxCount :: integer().
getAttachedShaders(Program,MaxCount) when is_integer(Program),is_integer(MaxCount) ->
  IF = get_interface(),
  IF:queue_cmd(Program,MaxCount,5460),
  rec(5460).

%% @doc glGetAttribLocation
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getAttribLocation(Program, Name) -> integer() when Program :: integer(),Name :: string().
getAttribLocation(Program,Name) when is_integer(Program),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,NameBin,5461),
  rec(5461).

%% @doc glGetProgram
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getProgramiv(Program, Pname) -> integer() when Program :: integer(),Pname :: enum().
getProgramiv(Program,Pname) when is_integer(Program),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Pname,5462),
  rec(5462).

%% @doc glGetProgramInfoLog
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getProgramInfoLog(Program, BufSize) -> string() when Program :: integer(),BufSize :: integer().
getProgramInfoLog(Program,BufSize) when is_integer(Program),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Program,BufSize,5463),
  rec(5463).

%% @doc glGetShader
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getShaderiv(Shader, Pname) -> integer() when Shader :: integer(),Pname :: enum().
getShaderiv(Shader,Pname) when is_integer(Shader),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Shader,Pname,5464),
  rec(5464).

%% @doc glGetShaderInfoLog
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getShaderInfoLog(Shader, BufSize) -> string() when Shader :: integer(),BufSize :: integer().
getShaderInfoLog(Shader,BufSize) when is_integer(Shader),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Shader,BufSize,5465),
  rec(5465).

%% @doc glGetShaderSource
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getShaderSource(Shader, BufSize) -> string() when Shader :: integer(),BufSize :: integer().
getShaderSource(Shader,BufSize) when is_integer(Shader),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Shader,BufSize,5466),
  rec(5466).

%% @doc glGetUniformLocation
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getUniformLocation(Program, Name) -> integer() when Program :: integer(),Name :: string().
getUniformLocation(Program,Name) when is_integer(Program),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,NameBin,5467),
  rec(5467).

%% @doc glGetUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getUniformfv(Program, Location) -> matrix() when Program :: integer(),Location :: integer().
getUniformfv(Program,Location) when is_integer(Program),is_integer(Location) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,5468),
  rec(5468).

%% @doc glGetUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getUniformiv(Program, Location) -> {integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer()} when Program :: integer(),Location :: integer().
getUniformiv(Program,Location) when is_integer(Program),is_integer(Location) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,5469),
  rec(5469).

%% @doc glGetVertexAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getVertexAttribdv(Index, Pname) -> {float(),float(),float(),float()} when Index :: integer(),Pname :: enum().
getVertexAttribdv(Index,Pname) when is_integer(Index),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Pname,5470),
  rec(5470).

%% @doc glGetVertexAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getVertexAttribfv(Index, Pname) -> {float(),float(),float(),float()} when Index :: integer(),Pname :: enum().
getVertexAttribfv(Index,Pname) when is_integer(Index),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Pname,5471),
  rec(5471).

%% @doc glGetVertexAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getVertexAttribiv(Index, Pname) -> {integer(),integer(),integer(),integer()} when Index :: integer(),Pname :: enum().
getVertexAttribiv(Index,Pname) when is_integer(Index),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Pname,5472),
  rec(5472).

%% @doc glIsProgram
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec isProgram(Program) -> 0|1 when Program :: integer().
isProgram(Program) when is_integer(Program) ->
  IF = get_interface(),
  IF:queue_cmd(Program,5473),
  rec(5473).

%% @doc glIsShader
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec isShader(Shader) -> 0|1 when Shader :: integer().
isShader(Shader) when is_integer(Shader) ->
  IF = get_interface(),
  IF:queue_cmd(Shader,5474),
  rec(5474).

%% @doc glLinkProgram
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec linkProgram(Program) -> 'ok' when Program :: integer().
linkProgram(Program) when is_integer(Program) ->
  IF = get_interface(),
  IF:queue_cmd(Program,5475),
  ok.

%% @doc glShaderSource
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec shaderSource(Shader, String) -> 'ok' when Shader :: integer(),String :: iolist().
shaderSource(Shader,String) when is_integer(Shader),is_list(String) ->
  IF = get_interface(),
  StringTemp = unicode:characters_to_binary([[Str|[0]] || Str <- String ]),
  Count = length(String),
  IF:queue_cmd(Shader,Count,StringTemp,5476),
  ok.

%% @doc glUseProgram
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec useProgram(Program) -> 'ok' when Program :: integer().
useProgram(Program) when is_integer(Program) ->
  IF = get_interface(),
  IF:queue_cmd(Program,5477),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniform1f(Location, V0) -> 'ok' when Location :: integer(),V0 :: float().
uniform1f(Location,V0) when is_integer(Location),is_float(V0) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,5478),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniform2f(Location, V0, V1) -> 'ok' when Location :: integer(),V0 :: float(),V1 :: float().
uniform2f(Location,V0,V1) when is_integer(Location),is_float(V0),is_float(V1) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,V1,5479),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniform3f(Location, V0, V1, V2) -> 'ok' when Location :: integer(),V0 :: float(),V1 :: float(),V2 :: float().
uniform3f(Location,V0,V1,V2) when is_integer(Location),is_float(V0),is_float(V1),is_float(V2) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,V1,V2,5480),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniform4f(Location, V0, V1, V2, V3) -> 'ok' when Location :: integer(),V0 :: float(),V1 :: float(),V2 :: float(),V3 :: float().
uniform4f(Location,V0,V1,V2,V3) when is_integer(Location),is_float(V0),is_float(V1),is_float(V2),is_float(V3) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,V1,V2,V3,5481),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniform1i(Location, V0) -> 'ok' when Location :: integer(),V0 :: integer().
uniform1i(Location,V0) when is_integer(Location),is_integer(V0) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,5482),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniform2i(Location, V0, V1) -> 'ok' when Location :: integer(),V0 :: integer(),V1 :: integer().
uniform2i(Location,V0,V1) when is_integer(Location),is_integer(V0),is_integer(V1) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,V1,5483),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniform3i(Location, V0, V1, V2) -> 'ok' when Location :: integer(),V0 :: integer(),V1 :: integer(),V2 :: integer().
uniform3i(Location,V0,V1,V2) when is_integer(Location),is_integer(V0),is_integer(V1),is_integer(V2) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,V1,V2,5484),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniform4i(Location, V0, V1, V2, V3) -> 'ok' when Location :: integer(),V0 :: integer(),V1 :: integer(),V2 :: integer(),V3 :: integer().
uniform4i(Location,V0,V1,V2,V3) when is_integer(Location),is_integer(V0),is_integer(V1),is_integer(V2),is_integer(V3) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,V1,V2,V3,5485),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniform1fv(Location, Value) -> 'ok' when Location :: integer(),Value :: [float()].
uniform1fv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5486),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniform2fv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{float(),float()}].
uniform2fv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5487),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniform3fv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{float(),float(),float()}].
uniform3fv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5488),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniform4fv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{float(),float(),float(),float()}].
uniform4fv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5489),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniform1iv(Location, Value) -> 'ok' when Location :: integer(),Value :: [integer()].
uniform1iv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5490),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniform2iv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{integer(),integer()}].
uniform2iv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5491),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniform3iv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{integer(),integer(),integer()}].
uniform3iv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5492),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniform4iv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{integer(),integer(),integer(),integer()}].
uniform4iv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5493),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniformMatrix2fv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float()}].
uniformMatrix2fv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5494),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniformMatrix3fv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix3fv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5495),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniformMatrix4fv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix4fv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5496),
  ok.

%% @doc glValidateProgram
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec validateProgram(Program) -> 'ok' when Program :: integer().
validateProgram(Program) when is_integer(Program) ->
  IF = get_interface(),
  IF:queue_cmd(Program,5497),
  ok.

%% @doc glVertexAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttrib1d(Index, X) -> 'ok' when Index :: integer(),X :: float().
vertexAttrib1d(Index,X) when is_integer(Index),is_float(X) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,5498),
  ok.

%% @equiv vertexAttrib1d(Index,X)
-spec vertexAttrib1dv(Index :: integer(),V) -> 'ok' when V :: {X :: float()}.
vertexAttrib1dv(Index,{X}) ->  vertexAttrib1d(Index,X).
%% @doc glVertexAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttrib1f(Index, X) -> 'ok' when Index :: integer(),X :: float().
vertexAttrib1f(Index,X) when is_integer(Index),is_float(X) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,5499),
  ok.

%% @equiv vertexAttrib1f(Index,X)
-spec vertexAttrib1fv(Index :: integer(),V) -> 'ok' when V :: {X :: float()}.
vertexAttrib1fv(Index,{X}) ->  vertexAttrib1f(Index,X).
%% @doc glVertexAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttrib1s(Index, X) -> 'ok' when Index :: integer(),X :: integer().
vertexAttrib1s(Index,X) when is_integer(Index),is_integer(X) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,5500),
  ok.

%% @equiv vertexAttrib1s(Index,X)
-spec vertexAttrib1sv(Index :: integer(),V) -> 'ok' when V :: {X :: integer()}.
vertexAttrib1sv(Index,{X}) ->  vertexAttrib1s(Index,X).
%% @doc glVertexAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttrib2d(Index, X, Y) -> 'ok' when Index :: integer(),X :: float(),Y :: float().
vertexAttrib2d(Index,X,Y) when is_integer(Index),is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,5501),
  ok.

%% @equiv vertexAttrib2d(Index,X,Y)
-spec vertexAttrib2dv(Index :: integer(),V) -> 'ok' when V :: {X :: float(),Y :: float()}.
vertexAttrib2dv(Index,{X,Y}) ->  vertexAttrib2d(Index,X,Y).
%% @doc glVertexAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttrib2f(Index, X, Y) -> 'ok' when Index :: integer(),X :: float(),Y :: float().
vertexAttrib2f(Index,X,Y) when is_integer(Index),is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,5502),
  ok.

%% @equiv vertexAttrib2f(Index,X,Y)
-spec vertexAttrib2fv(Index :: integer(),V) -> 'ok' when V :: {X :: float(),Y :: float()}.
vertexAttrib2fv(Index,{X,Y}) ->  vertexAttrib2f(Index,X,Y).
%% @doc glVertexAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttrib2s(Index, X, Y) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer().
vertexAttrib2s(Index,X,Y) when is_integer(Index),is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,5503),
  ok.

%% @equiv vertexAttrib2s(Index,X,Y)
-spec vertexAttrib2sv(Index :: integer(),V) -> 'ok' when V :: {X :: integer(),Y :: integer()}.
vertexAttrib2sv(Index,{X,Y}) ->  vertexAttrib2s(Index,X,Y).
%% @doc glVertexAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttrib3d(Index, X, Y, Z) -> 'ok' when Index :: integer(),X :: float(),Y :: float(),Z :: float().
vertexAttrib3d(Index,X,Y,Z) when is_integer(Index),is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,5504),
  ok.

%% @equiv vertexAttrib3d(Index,X,Y,Z)
-spec vertexAttrib3dv(Index :: integer(),V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float()}.
vertexAttrib3dv(Index,{X,Y,Z}) ->  vertexAttrib3d(Index,X,Y,Z).
%% @doc glVertexAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttrib3f(Index, X, Y, Z) -> 'ok' when Index :: integer(),X :: float(),Y :: float(),Z :: float().
vertexAttrib3f(Index,X,Y,Z) when is_integer(Index),is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,5505),
  ok.

%% @equiv vertexAttrib3f(Index,X,Y,Z)
-spec vertexAttrib3fv(Index :: integer(),V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float()}.
vertexAttrib3fv(Index,{X,Y,Z}) ->  vertexAttrib3f(Index,X,Y,Z).
%% @doc glVertexAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttrib3s(Index, X, Y, Z) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer(),Z :: integer().
vertexAttrib3s(Index,X,Y,Z) when is_integer(Index),is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,5506),
  ok.

%% @equiv vertexAttrib3s(Index,X,Y,Z)
-spec vertexAttrib3sv(Index :: integer(),V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer()}.
vertexAttrib3sv(Index,{X,Y,Z}) ->  vertexAttrib3s(Index,X,Y,Z).
%% @doc glVertexAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttrib4Nbv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4Nbv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5507),
  ok.

%% @doc glVertexAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttrib4Niv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4Niv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5508),
  ok.

%% @doc glVertexAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttrib4Nsv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4Nsv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5509),
  ok.

%% @doc glVertexAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttrib4Nub(Index, X, Y, Z, W) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer(),Z :: integer(),W :: integer().
vertexAttrib4Nub(Index,X,Y,Z,W) when is_integer(Index),is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,W,5510),
  ok.

%% @equiv vertexAttrib4Nub(Index,X,Y,Z,W)
-spec vertexAttrib4Nubv(Index :: integer(),V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer(),W :: integer()}.
vertexAttrib4Nubv(Index,{X,Y,Z,W}) ->  vertexAttrib4Nub(Index,X,Y,Z,W).
%% @doc glVertexAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttrib4Nuiv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4Nuiv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5511),
  ok.

%% @doc glVertexAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttrib4Nusv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4Nusv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5512),
  ok.

%% @doc glVertexAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttrib4bv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4bv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5513),
  ok.

%% @doc glVertexAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttrib4d(Index, X, Y, Z, W) -> 'ok' when Index :: integer(),X :: float(),Y :: float(),Z :: float(),W :: float().
vertexAttrib4d(Index,X,Y,Z,W) when is_integer(Index),is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,W,5514),
  ok.

%% @equiv vertexAttrib4d(Index,X,Y,Z,W)
-spec vertexAttrib4dv(Index :: integer(),V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float(),W :: float()}.
vertexAttrib4dv(Index,{X,Y,Z,W}) ->  vertexAttrib4d(Index,X,Y,Z,W).
%% @doc glVertexAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttrib4f(Index, X, Y, Z, W) -> 'ok' when Index :: integer(),X :: float(),Y :: float(),Z :: float(),W :: float().
vertexAttrib4f(Index,X,Y,Z,W) when is_integer(Index),is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,W,5515),
  ok.

%% @equiv vertexAttrib4f(Index,X,Y,Z,W)
-spec vertexAttrib4fv(Index :: integer(),V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float(),W :: float()}.
vertexAttrib4fv(Index,{X,Y,Z,W}) ->  vertexAttrib4f(Index,X,Y,Z,W).
%% @doc glVertexAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttrib4iv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4iv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5516),
  ok.

%% @doc glVertexAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttrib4s(Index, X, Y, Z, W) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer(),Z :: integer(),W :: integer().
vertexAttrib4s(Index,X,Y,Z,W) when is_integer(Index),is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,W,5517),
  ok.

%% @equiv vertexAttrib4s(Index,X,Y,Z,W)
-spec vertexAttrib4sv(Index :: integer(),V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer(),W :: integer()}.
vertexAttrib4sv(Index,{X,Y,Z,W}) ->  vertexAttrib4s(Index,X,Y,Z,W).
%% @doc glVertexAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttrib4ubv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4ubv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5518),
  ok.

%% @doc glVertexAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttrib4uiv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4uiv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5519),
  ok.

%% @doc glVertexAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttrib4usv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4usv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5520),
  ok.

%% @doc glVertexAttribPointer
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttribPointer(Index, Size, Type, Normalized, Stride, Pointer) -> 'ok' when Index :: integer(),Size :: integer(),Type :: enum(),Normalized :: 0|1,Stride :: integer(),Pointer :: offset()|mem().
vertexAttribPointer(Index,Size,Type,Normalized,Stride,Pointer) when is_integer(Index),is_integer(Size),is_integer(Type),(0 =:= Normalized) orelse (1 =:= Normalized),is_integer(Stride),is_integer(Pointer) orelse is_tuple(Pointer) orelse is_binary(Pointer) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Size,Type,Normalized,Stride,Pointer,5521),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniformMatrix2x3fv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float()}].
uniformMatrix2x3fv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5523),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniformMatrix3x2fv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float()}].
uniformMatrix3x2fv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5524),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniformMatrix2x4fv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix2x4fv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5525),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniformMatrix4x2fv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix4x2fv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5526),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniformMatrix3x4fv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix3x4fv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5527),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniformMatrix4x3fv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix4x3fv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5528),
  ok.

%% @doc glColorMaski
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec colorMaski(Index, R, G, B, A) -> 'ok' when Index :: integer(),R :: 0|1,G :: 0|1,B :: 0|1,A :: 0|1.
colorMaski(Index,R,G,B,A) when is_integer(Index),(0 =:= R) orelse (1 =:= R),(0 =:= G) orelse (1 =:= G),(0 =:= B) orelse (1 =:= B),(0 =:= A) orelse (1 =:= A) ->
  IF = get_interface(),
  IF:queue_cmd(Index,R,G,B,A,5529),
  ok.

%% @doc glGet
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getBooleani_v(Target, Index) -> [0|1] when Target :: enum(),Index :: integer().
getBooleani_v(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5530),
  rec(5530).

%% @doc glGet
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getIntegeri_v(Target, Index) -> [integer()] when Target :: enum(),Index :: integer().
getIntegeri_v(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5531),
  rec(5531).

%% @doc glEnable
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec enablei(Target, Index) -> 'ok' when Target :: enum(),Index :: integer().
enablei(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5532),
  ok.

%% @doc glEnablei
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec disablei(Target, Index) -> 'ok' when Target :: enum(),Index :: integer().
disablei(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5533),
  ok.

%% @doc glIsEnabledi
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec isEnabledi(Target, Index) -> 0|1 when Target :: enum(),Index :: integer().
isEnabledi(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5534),
  rec(5534).

%% @doc glBeginTransformFeedback
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec beginTransformFeedback(PrimitiveMode) -> 'ok' when PrimitiveMode :: enum().
beginTransformFeedback(PrimitiveMode) when is_integer(PrimitiveMode) ->
  IF = get_interface(),
  IF:queue_cmd(PrimitiveMode,5535),
  ok.

%% @doc glBeginTransformFeedback
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec endTransformFeedback() -> 'ok'.
endTransformFeedback()  ->
  IF = get_interface(),
  IF:queue_cmd(5536),
  ok.

%% @doc glBindBufferRange
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec bindBufferRange(Target, Index, Buffer, Offset, Size) -> 'ok' when Target :: enum(),Index :: integer(),Buffer :: integer(),Offset :: integer(),Size :: integer().
bindBufferRange(Target,Index,Buffer,Offset,Size) when is_integer(Target),is_integer(Index),is_integer(Buffer),is_integer(Offset),is_integer(Size) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,Buffer,Offset,Size,5537),
  ok.

%% @doc glBindBufferBase
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec bindBufferBase(Target, Index, Buffer) -> 'ok' when Target :: enum(),Index :: integer(),Buffer :: integer().
bindBufferBase(Target,Index,Buffer) when is_integer(Target),is_integer(Index),is_integer(Buffer) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,Buffer,5538),
  ok.

%% @doc glTransformFeedbackVaryings
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec transformFeedbackVaryings(Program, Varyings, BufferMode) -> 'ok' when Program :: integer(),Varyings :: iolist(),BufferMode :: enum().
transformFeedbackVaryings(Program,Varyings,BufferMode) when is_integer(Program),is_list(Varyings),is_integer(BufferMode) ->
  IF = get_interface(),
  VaryingsTemp = unicode:characters_to_binary([[Str|[0]] || Str <- Varyings ]),
  Count = length(Varyings),
  IF:queue_cmd(Program,Count,VaryingsTemp,BufferMode,5539),
  ok.

%% @doc glGetTransformFeedbackVarying
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getTransformFeedbackVarying(Program, Index, BufSize) -> {Size :: integer(),Type :: enum(),Name :: string()} when Program :: integer(),Index :: integer(),BufSize :: integer().
getTransformFeedbackVarying(Program,Index,BufSize) when is_integer(Program),is_integer(Index),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Index,BufSize,5540),
  rec(5540).

%% @doc glClampColor
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec clampColor(Target, Clamp) -> 'ok' when Target :: enum(),Clamp :: enum().
clampColor(Target,Clamp) when is_integer(Target),is_integer(Clamp) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Clamp,5541),
  ok.

%% @doc glBeginConditionalRender
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec beginConditionalRender(Id, Mode) -> 'ok' when Id :: integer(),Mode :: enum().
beginConditionalRender(Id,Mode) when is_integer(Id),is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Id,Mode,5542),
  ok.

%% @doc glBeginConditionalRender
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec endConditionalRender() -> 'ok'.
endConditionalRender()  ->
  IF = get_interface(),
  IF:queue_cmd(5543),
  ok.

%% @doc glVertexAttribIPointer
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttribIPointer(Index, Size, Type, Stride, Pointer) -> 'ok' when Index :: integer(),Size :: integer(),Type :: enum(),Stride :: integer(),Pointer :: offset()|mem().
vertexAttribIPointer(Index,Size,Type,Stride,Pointer) when is_integer(Index),is_integer(Size),is_integer(Type),is_integer(Stride),is_integer(Pointer) orelse is_tuple(Pointer) orelse is_binary(Pointer) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Size,Type,Stride,Pointer,5544),
  ok.

%% @doc glGetVertexAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getVertexAttribIiv(Index, Pname) -> {integer(),integer(),integer(),integer()} when Index :: integer(),Pname :: enum().
getVertexAttribIiv(Index,Pname) when is_integer(Index),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Pname,5546),
  rec(5546).

%% @doc glGetVertexAttribI
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getVertexAttribIuiv(Index, Pname) -> {integer(),integer(),integer(),integer()} when Index :: integer(),Pname :: enum().
getVertexAttribIuiv(Index,Pname) when is_integer(Index),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Pname,5547),
  rec(5547).

%% @doc glVertexAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttribI1i(Index, X) -> 'ok' when Index :: integer(),X :: integer().
vertexAttribI1i(Index,X) when is_integer(Index),is_integer(X) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,5548),
  ok.

%% @doc glVertexAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttribI2i(Index, X, Y) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer().
vertexAttribI2i(Index,X,Y) when is_integer(Index),is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,5549),
  ok.

%% @doc glVertexAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttribI3i(Index, X, Y, Z) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer(),Z :: integer().
vertexAttribI3i(Index,X,Y,Z) when is_integer(Index),is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,5550),
  ok.

%% @doc glVertexAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttribI4i(Index, X, Y, Z, W) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer(),Z :: integer(),W :: integer().
vertexAttribI4i(Index,X,Y,Z,W) when is_integer(Index),is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,W,5551),
  ok.

%% @doc glVertexAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttribI1ui(Index, X) -> 'ok' when Index :: integer(),X :: integer().
vertexAttribI1ui(Index,X) when is_integer(Index),is_integer(X) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,5552),
  ok.

%% @doc glVertexAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttribI2ui(Index, X, Y) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer().
vertexAttribI2ui(Index,X,Y) when is_integer(Index),is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,5553),
  ok.

%% @doc glVertexAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttribI3ui(Index, X, Y, Z) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer(),Z :: integer().
vertexAttribI3ui(Index,X,Y,Z) when is_integer(Index),is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,5554),
  ok.

%% @doc glVertexAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttribI4ui(Index, X, Y, Z, W) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer(),Z :: integer(),W :: integer().
vertexAttribI4ui(Index,X,Y,Z,W) when is_integer(Index),is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,W,5555),
  ok.

%% @equiv vertexAttribI1i(Index,X)
-spec vertexAttribI1iv(Index :: integer(),V) -> 'ok' when V :: {X :: integer()}.
vertexAttribI1iv(Index,{X}) ->  vertexAttribI1i(Index,X).
%% @equiv vertexAttribI2i(Index,X,Y)
-spec vertexAttribI2iv(Index :: integer(),V) -> 'ok' when V :: {X :: integer(),Y :: integer()}.
vertexAttribI2iv(Index,{X,Y}) ->  vertexAttribI2i(Index,X,Y).
%% @equiv vertexAttribI3i(Index,X,Y,Z)
-spec vertexAttribI3iv(Index :: integer(),V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer()}.
vertexAttribI3iv(Index,{X,Y,Z}) ->  vertexAttribI3i(Index,X,Y,Z).
%% @equiv vertexAttribI4i(Index,X,Y,Z,W)
-spec vertexAttribI4iv(Index :: integer(),V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer(),W :: integer()}.
vertexAttribI4iv(Index,{X,Y,Z,W}) ->  vertexAttribI4i(Index,X,Y,Z,W).
%% @equiv vertexAttribI1ui(Index,X)
-spec vertexAttribI1uiv(Index :: integer(),V) -> 'ok' when V :: {X :: integer()}.
vertexAttribI1uiv(Index,{X}) ->  vertexAttribI1ui(Index,X).
%% @equiv vertexAttribI2ui(Index,X,Y)
-spec vertexAttribI2uiv(Index :: integer(),V) -> 'ok' when V :: {X :: integer(),Y :: integer()}.
vertexAttribI2uiv(Index,{X,Y}) ->  vertexAttribI2ui(Index,X,Y).
%% @equiv vertexAttribI3ui(Index,X,Y,Z)
-spec vertexAttribI3uiv(Index :: integer(),V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer()}.
vertexAttribI3uiv(Index,{X,Y,Z}) ->  vertexAttribI3ui(Index,X,Y,Z).
%% @equiv vertexAttribI4ui(Index,X,Y,Z,W)
-spec vertexAttribI4uiv(Index :: integer(),V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer(),W :: integer()}.
vertexAttribI4uiv(Index,{X,Y,Z,W}) ->  vertexAttribI4ui(Index,X,Y,Z,W).
%% @doc glVertexAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttribI4bv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttribI4bv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5556),
  ok.

%% @doc glVertexAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttribI4sv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttribI4sv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5557),
  ok.

%% @doc glVertexAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttribI4ubv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttribI4ubv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5558),
  ok.

%% @doc glVertexAttrib
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttribI4usv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttribI4usv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5559),
  ok.

%% @doc glGetUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getUniformuiv(Program, Location) -> {integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer()} when Program :: integer(),Location :: integer().
getUniformuiv(Program,Location) when is_integer(Program),is_integer(Location) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,5560),
  rec(5560).

%% @doc glBindFragDataLocation
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec bindFragDataLocation(Program, Color, Name) -> 'ok' when Program :: integer(),Color :: integer(),Name :: string().
bindFragDataLocation(Program,Color,Name) when is_integer(Program),is_integer(Color),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,Color,NameBin,5561),
  ok.

%% @doc glGetFragDataLocation
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getFragDataLocation(Program, Name) -> integer() when Program :: integer(),Name :: string().
getFragDataLocation(Program,Name) when is_integer(Program),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,NameBin,5562),
  rec(5562).

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniform1ui(Location, V0) -> 'ok' when Location :: integer(),V0 :: integer().
uniform1ui(Location,V0) when is_integer(Location),is_integer(V0) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,5563),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniform2ui(Location, V0, V1) -> 'ok' when Location :: integer(),V0 :: integer(),V1 :: integer().
uniform2ui(Location,V0,V1) when is_integer(Location),is_integer(V0),is_integer(V1) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,V1,5564),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniform3ui(Location, V0, V1, V2) -> 'ok' when Location :: integer(),V0 :: integer(),V1 :: integer(),V2 :: integer().
uniform3ui(Location,V0,V1,V2) when is_integer(Location),is_integer(V0),is_integer(V1),is_integer(V2) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,V1,V2,5565),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniform4ui(Location, V0, V1, V2, V3) -> 'ok' when Location :: integer(),V0 :: integer(),V1 :: integer(),V2 :: integer(),V3 :: integer().
uniform4ui(Location,V0,V1,V2,V3) when is_integer(Location),is_integer(V0),is_integer(V1),is_integer(V2),is_integer(V3) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,V1,V2,V3,5566),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniform1uiv(Location, Value) -> 'ok' when Location :: integer(),Value :: [integer()].
uniform1uiv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5567),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniform2uiv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{integer(),integer()}].
uniform2uiv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5568),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniform3uiv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{integer(),integer(),integer()}].
uniform3uiv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5569),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniform4uiv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{integer(),integer(),integer(),integer()}].
uniform4uiv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5570),
  ok.

%% @doc glTexParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texParameterIiv(Target, Pname, Params) -> 'ok' when Target :: enum(),Pname :: enum(),Params :: tuple().
texParameterIiv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5571),
  ok.

%% @doc glTexParameterI
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texParameterIuiv(Target, Pname, Params) -> 'ok' when Target :: enum(),Pname :: enum(),Params :: tuple().
texParameterIuiv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5572),
  ok.

%% @doc glGetTexParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getTexParameterIiv(Target, Pname) -> {integer(),integer(),integer(),integer()} when Target :: enum(),Pname :: enum().
getTexParameterIiv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5573),
  rec(5573).

%% @doc glGetTexParameterI
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getTexParameterIuiv(Target, Pname) -> {integer(),integer(),integer(),integer()} when Target :: enum(),Pname :: enum().
getTexParameterIuiv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5574),
  rec(5574).

%% @doc glClearBuffer
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec clearBufferiv(Buffer, Drawbuffer, Value) -> 'ok' when Buffer :: enum(),Drawbuffer :: integer(),Value :: tuple().
clearBufferiv(Buffer,Drawbuffer,Value) when is_integer(Buffer),is_integer(Drawbuffer),is_tuple(Value) ->
  IF = get_interface(),
  IF:queue_cmd(Buffer,Drawbuffer,Value,5575),
  ok.

%% @doc glClearBuffer
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec clearBufferuiv(Buffer, Drawbuffer, Value) -> 'ok' when Buffer :: enum(),Drawbuffer :: integer(),Value :: tuple().
clearBufferuiv(Buffer,Drawbuffer,Value) when is_integer(Buffer),is_integer(Drawbuffer),is_tuple(Value) ->
  IF = get_interface(),
  IF:queue_cmd(Buffer,Drawbuffer,Value,5576),
  ok.

%% @doc glClearBuffer
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec clearBufferfv(Buffer, Drawbuffer, Value) -> 'ok' when Buffer :: enum(),Drawbuffer :: integer(),Value :: tuple().
clearBufferfv(Buffer,Drawbuffer,Value) when is_integer(Buffer),is_integer(Drawbuffer),is_tuple(Value) ->
  IF = get_interface(),
  IF:queue_cmd(Buffer,Drawbuffer,Value,5577),
  ok.

%% @doc glClearBufferfi
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec clearBufferfi(Buffer, Drawbuffer, Depth, Stencil) -> 'ok' when Buffer :: enum(),Drawbuffer :: integer(),Depth :: float(),Stencil :: integer().
clearBufferfi(Buffer,Drawbuffer,Depth,Stencil) when is_integer(Buffer),is_integer(Drawbuffer),is_float(Depth),is_integer(Stencil) ->
  IF = get_interface(),
  IF:queue_cmd(Buffer,Drawbuffer,Depth,Stencil,5578),
  ok.

%% @doc glGetString
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getStringi(Name, Index) -> string() when Name :: enum(),Index :: integer().
getStringi(Name,Index) when is_integer(Name),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Name,Index,5579),
  rec(5579).

%% @doc glDrawArraysInstance
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec drawArraysInstanced(Mode, First, Count, Primcount) -> 'ok' when Mode :: enum(),First :: integer(),Count :: integer(),Primcount :: integer().
drawArraysInstanced(Mode,First,Count,Primcount) when is_integer(Mode),is_integer(First),is_integer(Count),is_integer(Primcount) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,First,Count,Primcount,5580),
  ok.

%% @doc glDrawElementsInstance
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec drawElementsInstanced(Mode, Count, Type, Indices, Primcount) -> 'ok' when Mode :: enum(),Count :: integer(),Type :: enum(),Indices :: offset()|mem(),Primcount :: integer().
drawElementsInstanced(Mode,Count,Type,Indices,Primcount) when is_integer(Mode),is_integer(Count),is_integer(Type),is_integer(Indices) orelse is_tuple(Indices) orelse is_binary(Indices),is_integer(Primcount) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Count,Type,Indices,Primcount,5581),
  ok.

%% @doc glTexBuffer
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texBuffer(Target, Internalformat, Buffer) -> 'ok' when Target :: enum(),Internalformat :: enum(),Buffer :: integer().
texBuffer(Target,Internalformat,Buffer) when is_integer(Target),is_integer(Internalformat),is_integer(Buffer) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Buffer,5583),
  ok.

%% @doc glPrimitiveRestartIndex
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec primitiveRestartIndex(Index) -> 'ok' when Index :: integer().
primitiveRestartIndex(Index) when is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Index,5584),
  ok.

%% @doc glGet
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getInteger64i_v(Target, Index) -> [integer()] when Target :: enum(),Index :: integer().
getInteger64i_v(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5585),
  rec(5585).

%% @doc glGetBufferParameteri64v
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getBufferParameteri64v(Target, Pname) -> [integer()] when Target :: enum(),Pname :: enum().
getBufferParameteri64v(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5586),
  rec(5586).

%% @doc glFramebufferTexture
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec framebufferTexture(Target, Attachment, Texture, Level) -> 'ok' when Target :: enum(),Attachment :: enum(),Texture :: integer(),Level :: integer().
framebufferTexture(Target,Attachment,Texture,Level) when is_integer(Target),is_integer(Attachment),is_integer(Texture),is_integer(Level) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Attachment,Texture,Level,5587),
  ok.

%% @doc glVertexAttribDivisor
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttribDivisor(Index, Divisor) -> 'ok' when Index :: integer(),Divisor :: integer().
vertexAttribDivisor(Index,Divisor) when is_integer(Index),is_integer(Divisor) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Divisor,5588),
  ok.

%% @doc glMinSampleShading
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec minSampleShading(Value) -> 'ok' when Value :: clamp().
minSampleShading(Value) when is_float(Value) ->
  IF = get_interface(),
  IF:queue_cmd(Value,5589),
  ok.

%% @doc glBlendEquation
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec blendEquationi(Buf, Mode) -> 'ok' when Buf :: integer(),Mode :: enum().
blendEquationi(Buf,Mode) when is_integer(Buf),is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Buf,Mode,5590),
  ok.

%% @doc glBlendEquationSeparate
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec blendEquationSeparatei(Buf, ModeRGB, ModeAlpha) -> 'ok' when Buf :: integer(),ModeRGB :: enum(),ModeAlpha :: enum().
blendEquationSeparatei(Buf,ModeRGB,ModeAlpha) when is_integer(Buf),is_integer(ModeRGB),is_integer(ModeAlpha) ->
  IF = get_interface(),
  IF:queue_cmd(Buf,ModeRGB,ModeAlpha,5591),
  ok.

%% @doc glBlendFunci
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec blendFunci(Buf, Src, Dst) -> 'ok' when Buf :: integer(),Src :: enum(),Dst :: enum().
blendFunci(Buf,Src,Dst) when is_integer(Buf),is_integer(Src),is_integer(Dst) ->
  IF = get_interface(),
  IF:queue_cmd(Buf,Src,Dst,5592),
  ok.

%% @doc glBlendFuncSeparate
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec blendFuncSeparatei(Buf, SrcRGB, DstRGB, SrcAlpha, DstAlpha) -> 'ok' when Buf :: integer(),SrcRGB :: enum(),DstRGB :: enum(),SrcAlpha :: enum(),DstAlpha :: enum().
blendFuncSeparatei(Buf,SrcRGB,DstRGB,SrcAlpha,DstAlpha) when is_integer(Buf),is_integer(SrcRGB),is_integer(DstRGB),is_integer(SrcAlpha),is_integer(DstAlpha) ->
  IF = get_interface(),
  IF:queue_cmd(Buf,SrcRGB,DstRGB,SrcAlpha,DstAlpha,5593),
  ok.

%% @doc glLoadTransposeMatrixARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec loadTransposeMatrixfARB(M) -> 'ok' when M :: matrix().
loadTransposeMatrixfARB(M) when tuple_size(M) =:= 16 ->
  IF = get_interface(),
  IF:queue_cmd(M,5594),
  ok.

%% @doc glLoadTransposeMatrixARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec loadTransposeMatrixdARB(M) -> 'ok' when M :: matrix().
loadTransposeMatrixdARB(M) when tuple_size(M) =:= 16 ->
  IF = get_interface(),
  IF:queue_cmd(M,5595),
  ok.

%% @doc glMultTransposeMatrixARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec multTransposeMatrixfARB(M) -> 'ok' when M :: matrix().
multTransposeMatrixfARB(M) when tuple_size(M) =:= 16 ->
  IF = get_interface(),
  IF:queue_cmd(M,5596),
  ok.

%% @doc glMultTransposeMatrixARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec multTransposeMatrixdARB(M) -> 'ok' when M :: matrix().
multTransposeMatrixdARB(M) when tuple_size(M) =:= 16 ->
  IF = get_interface(),
  IF:queue_cmd(M,5597),
  ok.

%% @doc glWeightARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec weightbvARB(Weights) -> 'ok' when Weights :: [integer()].
weightbvARB(Weights) when is_list(Weights) ->
  IF = get_interface(),
  Size = length(Weights),
  IF:queue_cmd(Size,Weights,5598),
  ok.

%% @doc glWeightARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec weightsvARB(Weights) -> 'ok' when Weights :: [integer()].
weightsvARB(Weights) when is_list(Weights) ->
  IF = get_interface(),
  Size = length(Weights),
  IF:queue_cmd(Size,Weights,5599),
  ok.

%% @doc glWeightARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec weightivARB(Weights) -> 'ok' when Weights :: [integer()].
weightivARB(Weights) when is_list(Weights) ->
  IF = get_interface(),
  Size = length(Weights),
  IF:queue_cmd(Size,Weights,5600),
  ok.

%% @doc glWeightARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec weightfvARB(Weights) -> 'ok' when Weights :: [float()].
weightfvARB(Weights) when is_list(Weights) ->
  IF = get_interface(),
  Size = length(Weights),
  IF:queue_cmd(Size,Weights,5601),
  ok.

%% @doc glWeightARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec weightdvARB(Weights) -> 'ok' when Weights :: [float()].
weightdvARB(Weights) when is_list(Weights) ->
  IF = get_interface(),
  Size = length(Weights),
  IF:queue_cmd(Size,Weights,5602),
  ok.

%% @doc glWeightARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec weightubvARB(Weights) -> 'ok' when Weights :: [integer()].
weightubvARB(Weights) when is_list(Weights) ->
  IF = get_interface(),
  Size = length(Weights),
  IF:queue_cmd(Size,Weights,5603),
  ok.

%% @doc glWeightARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec weightusvARB(Weights) -> 'ok' when Weights :: [integer()].
weightusvARB(Weights) when is_list(Weights) ->
  IF = get_interface(),
  Size = length(Weights),
  IF:queue_cmd(Size,Weights,5604),
  ok.

%% @doc glWeightARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec weightuivARB(Weights) -> 'ok' when Weights :: [integer()].
weightuivARB(Weights) when is_list(Weights) ->
  IF = get_interface(),
  Size = length(Weights),
  IF:queue_cmd(Size,Weights,5605),
  ok.

%% @doc glVertexBlenARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexBlendARB(Count) -> 'ok' when Count :: integer().
vertexBlendARB(Count) when is_integer(Count) ->
  IF = get_interface(),
  IF:queue_cmd(Count,5606),
  ok.

%% @doc glCurrentPaletteMatrixARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec currentPaletteMatrixARB(Index) -> 'ok' when Index :: integer().
currentPaletteMatrixARB(Index) when is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Index,5607),
  ok.

%% @doc glMatrixIndexARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec matrixIndexubvARB(Indices) -> 'ok' when Indices :: [integer()].
matrixIndexubvARB(Indices) when is_list(Indices) ->
  IF = get_interface(),
  Size = length(Indices),
  IF:queue_cmd(Size,Indices,5608),
  ok.

%% @doc glMatrixIndexARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec matrixIndexusvARB(Indices) -> 'ok' when Indices :: [integer()].
matrixIndexusvARB(Indices) when is_list(Indices) ->
  IF = get_interface(),
  Size = length(Indices),
  IF:queue_cmd(Size,Indices,5609),
  ok.

%% @doc glMatrixIndexARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec matrixIndexuivARB(Indices) -> 'ok' when Indices :: [integer()].
matrixIndexuivARB(Indices) when is_list(Indices) ->
  IF = get_interface(),
  Size = length(Indices),
  IF:queue_cmd(Size,Indices,5610),
  ok.

%% @doc glProgramStringARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programStringARB(Target, Format, String) -> 'ok' when Target :: enum(),Format :: enum(),String :: string().
programStringARB(Target,Format,String) when is_integer(Target),is_integer(Format),is_list(String) ->
  IF = get_interface(),
  StringBin = unicode:characters_to_binary([String|[0]]),
  IF:queue_cmd(Target,Format,StringBin,5611),
  ok.

%% @doc glBindProgramARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec bindProgramARB(Target, Program) -> 'ok' when Target :: enum(),Program :: integer().
bindProgramARB(Target,Program) when is_integer(Target),is_integer(Program) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Program,5612),
  ok.

%% @doc glDeleteProgramsARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec deleteProgramsARB(Programs) -> 'ok' when Programs :: [integer()].
deleteProgramsARB(Programs) when is_list(Programs) ->
  IF = get_interface(),
  N = length(Programs),
  IF:queue_cmd(N,Programs,5613),
  ok.

%% @doc glGenProgramsARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec genProgramsARB(N) -> [integer()] when N :: integer().
genProgramsARB(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5614),
  rec(5614).

%% @doc glProgramEnvParameterARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programEnvParameter4dARB(Target, Index, X, Y, Z, W) -> 'ok' when Target :: enum(),Index :: integer(),X :: float(),Y :: float(),Z :: float(),W :: float().
programEnvParameter4dARB(Target,Index,X,Y,Z,W) when is_integer(Target),is_integer(Index),is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,X,Y,Z,W,5615),
  ok.

%% @doc glProgramEnvParameterARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programEnvParameter4dvARB(Target, Index, Params) -> 'ok' when Target :: enum(),Index :: integer(),Params :: {float(),float(),float(),float()}.
programEnvParameter4dvARB(Target,Index,Params) when is_integer(Target),is_integer(Index),tuple_size(Params) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,Params,5616),
  ok.

%% @doc glProgramEnvParameterARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programEnvParameter4fARB(Target, Index, X, Y, Z, W) -> 'ok' when Target :: enum(),Index :: integer(),X :: float(),Y :: float(),Z :: float(),W :: float().
programEnvParameter4fARB(Target,Index,X,Y,Z,W) when is_integer(Target),is_integer(Index),is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,X,Y,Z,W,5617),
  ok.

%% @doc glProgramEnvParameterARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programEnvParameter4fvARB(Target, Index, Params) -> 'ok' when Target :: enum(),Index :: integer(),Params :: {float(),float(),float(),float()}.
programEnvParameter4fvARB(Target,Index,Params) when is_integer(Target),is_integer(Index),tuple_size(Params) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,Params,5618),
  ok.

%% @doc glProgramLocalParameterARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programLocalParameter4dARB(Target, Index, X, Y, Z, W) -> 'ok' when Target :: enum(),Index :: integer(),X :: float(),Y :: float(),Z :: float(),W :: float().
programLocalParameter4dARB(Target,Index,X,Y,Z,W) when is_integer(Target),is_integer(Index),is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,X,Y,Z,W,5619),
  ok.

%% @doc glProgramLocalParameterARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programLocalParameter4dvARB(Target, Index, Params) -> 'ok' when Target :: enum(),Index :: integer(),Params :: {float(),float(),float(),float()}.
programLocalParameter4dvARB(Target,Index,Params) when is_integer(Target),is_integer(Index),tuple_size(Params) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,Params,5620),
  ok.

%% @doc glProgramLocalParameterARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programLocalParameter4fARB(Target, Index, X, Y, Z, W) -> 'ok' when Target :: enum(),Index :: integer(),X :: float(),Y :: float(),Z :: float(),W :: float().
programLocalParameter4fARB(Target,Index,X,Y,Z,W) when is_integer(Target),is_integer(Index),is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,X,Y,Z,W,5621),
  ok.

%% @doc glProgramLocalParameterARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programLocalParameter4fvARB(Target, Index, Params) -> 'ok' when Target :: enum(),Index :: integer(),Params :: {float(),float(),float(),float()}.
programLocalParameter4fvARB(Target,Index,Params) when is_integer(Target),is_integer(Index),tuple_size(Params) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,Params,5622),
  ok.

%% @doc glGetProgramEnvParameterARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getProgramEnvParameterdvARB(Target, Index) -> {float(),float(),float(),float()} when Target :: enum(),Index :: integer().
getProgramEnvParameterdvARB(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5623),
  rec(5623).

%% @doc glGetProgramEnvParameterARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getProgramEnvParameterfvARB(Target, Index) -> {float(),float(),float(),float()} when Target :: enum(),Index :: integer().
getProgramEnvParameterfvARB(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5624),
  rec(5624).

%% @doc glGetProgramLocalParameterARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getProgramLocalParameterdvARB(Target, Index) -> {float(),float(),float(),float()} when Target :: enum(),Index :: integer().
getProgramLocalParameterdvARB(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5625),
  rec(5625).

%% @doc glGetProgramLocalParameterARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getProgramLocalParameterfvARB(Target, Index) -> {float(),float(),float(),float()} when Target :: enum(),Index :: integer().
getProgramLocalParameterfvARB(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5626),
  rec(5626).

%% @doc glGetProgramStringARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getProgramStringARB(Target, Pname, String) -> 'ok' when Target :: enum(),Pname :: enum(),String :: mem().
getProgramStringARB(Target,Pname,String) when is_integer(Target),is_integer(Pname),is_tuple(String) orelse is_binary(String) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,String,5627),
  rec(5627).

%% @doc glGetBufferParameterARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getBufferParameterivARB(Target, Pname) -> [integer()] when Target :: enum(),Pname :: enum().
getBufferParameterivARB(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5628),
  rec(5628).

%% @doc glDeleteObjectARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec deleteObjectARB(Obj) -> 'ok' when Obj :: integer().
deleteObjectARB(Obj) when is_integer(Obj) ->
  IF = get_interface(),
  IF:queue_cmd(Obj,5629),
  ok.

%% @doc glGetHandleARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getHandleARB(Pname) -> integer() when Pname :: enum().
getHandleARB(Pname) when is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,5630),
  rec(5630).

%% @doc glDetachObjectARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec detachObjectARB(ContainerObj, AttachedObj) -> 'ok' when ContainerObj :: integer(),AttachedObj :: integer().
detachObjectARB(ContainerObj,AttachedObj) when is_integer(ContainerObj),is_integer(AttachedObj) ->
  IF = get_interface(),
  IF:queue_cmd(ContainerObj,AttachedObj,5631),
  ok.

%% @doc glCreateShaderObjectARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec createShaderObjectARB(ShaderType) -> integer() when ShaderType :: enum().
createShaderObjectARB(ShaderType) when is_integer(ShaderType) ->
  IF = get_interface(),
  IF:queue_cmd(ShaderType,5632),
  rec(5632).

%% @doc glShaderSourceARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec shaderSourceARB(ShaderObj, String) -> 'ok' when ShaderObj :: integer(),String :: iolist().
shaderSourceARB(ShaderObj,String) when is_integer(ShaderObj),is_list(String) ->
  IF = get_interface(),
  StringTemp = unicode:characters_to_binary([[Str|[0]] || Str <- String ]),
  Count = length(String),
  IF:queue_cmd(ShaderObj,Count,StringTemp,5633),
  ok.

%% @doc glCompileShaderARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec compileShaderARB(ShaderObj) -> 'ok' when ShaderObj :: integer().
compileShaderARB(ShaderObj) when is_integer(ShaderObj) ->
  IF = get_interface(),
  IF:queue_cmd(ShaderObj,5634),
  ok.

%% @doc glCreateProgramObjectARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec createProgramObjectARB() -> integer().
createProgramObjectARB()  ->
  IF = get_interface(),
  IF:queue_cmd(5635),
  rec(5635).

%% @doc glAttachObjectARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec attachObjectARB(ContainerObj, Obj) -> 'ok' when ContainerObj :: integer(),Obj :: integer().
attachObjectARB(ContainerObj,Obj) when is_integer(ContainerObj),is_integer(Obj) ->
  IF = get_interface(),
  IF:queue_cmd(ContainerObj,Obj,5636),
  ok.

%% @doc glLinkProgramARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec linkProgramARB(ProgramObj) -> 'ok' when ProgramObj :: integer().
linkProgramARB(ProgramObj) when is_integer(ProgramObj) ->
  IF = get_interface(),
  IF:queue_cmd(ProgramObj,5637),
  ok.

%% @doc glUseProgramObjectARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec useProgramObjectARB(ProgramObj) -> 'ok' when ProgramObj :: integer().
useProgramObjectARB(ProgramObj) when is_integer(ProgramObj) ->
  IF = get_interface(),
  IF:queue_cmd(ProgramObj,5638),
  ok.

%% @doc glValidateProgramARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec validateProgramARB(ProgramObj) -> 'ok' when ProgramObj :: integer().
validateProgramARB(ProgramObj) when is_integer(ProgramObj) ->
  IF = get_interface(),
  IF:queue_cmd(ProgramObj,5639),
  ok.

%% @doc glGetObjectParameterARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getObjectParameterfvARB(Obj, Pname) -> float() when Obj :: integer(),Pname :: enum().
getObjectParameterfvARB(Obj,Pname) when is_integer(Obj),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Obj,Pname,5640),
  rec(5640).

%% @doc glGetObjectParameterARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getObjectParameterivARB(Obj, Pname) -> integer() when Obj :: integer(),Pname :: enum().
getObjectParameterivARB(Obj,Pname) when is_integer(Obj),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Obj,Pname,5641),
  rec(5641).

%% @doc glGetInfoLogARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getInfoLogARB(Obj, MaxLength) -> string() when Obj :: integer(),MaxLength :: integer().
getInfoLogARB(Obj,MaxLength) when is_integer(Obj),is_integer(MaxLength) ->
  IF = get_interface(),
  IF:queue_cmd(Obj,MaxLength,5642),
  rec(5642).

%% @doc glGetAttachedObjectsARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getAttachedObjectsARB(ContainerObj, MaxCount) -> [integer()] when ContainerObj :: integer(),MaxCount :: integer().
getAttachedObjectsARB(ContainerObj,MaxCount) when is_integer(ContainerObj),is_integer(MaxCount) ->
  IF = get_interface(),
  IF:queue_cmd(ContainerObj,MaxCount,5643),
  rec(5643).

%% @doc glGetUniformLocationARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getUniformLocationARB(ProgramObj, Name) -> integer() when ProgramObj :: integer(),Name :: string().
getUniformLocationARB(ProgramObj,Name) when is_integer(ProgramObj),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(ProgramObj,NameBin,5644),
  rec(5644).

%% @doc glGetActiveUniformARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getActiveUniformARB(ProgramObj, Index, MaxLength) -> {Size :: integer(),Type :: enum(),Name :: string()} when ProgramObj :: integer(),Index :: integer(),MaxLength :: integer().
getActiveUniformARB(ProgramObj,Index,MaxLength) when is_integer(ProgramObj),is_integer(Index),is_integer(MaxLength) ->
  IF = get_interface(),
  IF:queue_cmd(ProgramObj,Index,MaxLength,5645),
  rec(5645).

%% @doc glGetUniformARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getUniformfvARB(ProgramObj, Location) -> matrix() when ProgramObj :: integer(),Location :: integer().
getUniformfvARB(ProgramObj,Location) when is_integer(ProgramObj),is_integer(Location) ->
  IF = get_interface(),
  IF:queue_cmd(ProgramObj,Location,5646),
  rec(5646).

%% @doc glGetUniformARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getUniformivARB(ProgramObj, Location) -> {integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer()} when ProgramObj :: integer(),Location :: integer().
getUniformivARB(ProgramObj,Location) when is_integer(ProgramObj),is_integer(Location) ->
  IF = get_interface(),
  IF:queue_cmd(ProgramObj,Location,5647),
  rec(5647).

%% @doc glGetShaderSourceARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getShaderSourceARB(Obj, MaxLength) -> string() when Obj :: integer(),MaxLength :: integer().
getShaderSourceARB(Obj,MaxLength) when is_integer(Obj),is_integer(MaxLength) ->
  IF = get_interface(),
  IF:queue_cmd(Obj,MaxLength,5648),
  rec(5648).

%% @doc glBindAttribLocationARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec bindAttribLocationARB(ProgramObj, Index, Name) -> 'ok' when ProgramObj :: integer(),Index :: integer(),Name :: string().
bindAttribLocationARB(ProgramObj,Index,Name) when is_integer(ProgramObj),is_integer(Index),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(ProgramObj,Index,NameBin,5649),
  ok.

%% @doc glGetActiveAttribARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getActiveAttribARB(ProgramObj, Index, MaxLength) -> {Size :: integer(),Type :: enum(),Name :: string()} when ProgramObj :: integer(),Index :: integer(),MaxLength :: integer().
getActiveAttribARB(ProgramObj,Index,MaxLength) when is_integer(ProgramObj),is_integer(Index),is_integer(MaxLength) ->
  IF = get_interface(),
  IF:queue_cmd(ProgramObj,Index,MaxLength,5650),
  rec(5650).

%% @doc glGetAttribLocationARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getAttribLocationARB(ProgramObj, Name) -> integer() when ProgramObj :: integer(),Name :: string().
getAttribLocationARB(ProgramObj,Name) when is_integer(ProgramObj),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(ProgramObj,NameBin,5651),
  rec(5651).

%% @doc glIsRenderbuffer
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec isRenderbuffer(Renderbuffer) -> 0|1 when Renderbuffer :: integer().
isRenderbuffer(Renderbuffer) when is_integer(Renderbuffer) ->
  IF = get_interface(),
  IF:queue_cmd(Renderbuffer,5652),
  rec(5652).

%% @doc glBindRenderbuffer
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec bindRenderbuffer(Target, Renderbuffer) -> 'ok' when Target :: enum(),Renderbuffer :: integer().
bindRenderbuffer(Target,Renderbuffer) when is_integer(Target),is_integer(Renderbuffer) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Renderbuffer,5653),
  ok.

%% @doc glDeleteRenderbuffers
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec deleteRenderbuffers(Renderbuffers) -> 'ok' when Renderbuffers :: [integer()].
deleteRenderbuffers(Renderbuffers) when is_list(Renderbuffers) ->
  IF = get_interface(),
  N = length(Renderbuffers),
  IF:queue_cmd(N,Renderbuffers,5654),
  ok.

%% @doc glGenRenderbuffers
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec genRenderbuffers(N) -> [integer()] when N :: integer().
genRenderbuffers(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5655),
  rec(5655).

%% @doc glRenderbufferStorage
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec renderbufferStorage(Target, Internalformat, Width, Height) -> 'ok' when Target :: enum(),Internalformat :: enum(),Width :: integer(),Height :: integer().
renderbufferStorage(Target,Internalformat,Width,Height) when is_integer(Target),is_integer(Internalformat),is_integer(Width),is_integer(Height) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Width,Height,5656),
  ok.

%% @doc glGetRenderbufferParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getRenderbufferParameteriv(Target, Pname) -> integer() when Target :: enum(),Pname :: enum().
getRenderbufferParameteriv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5657),
  rec(5657).

%% @doc glIsFramebuffer
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec isFramebuffer(Framebuffer) -> 0|1 when Framebuffer :: integer().
isFramebuffer(Framebuffer) when is_integer(Framebuffer) ->
  IF = get_interface(),
  IF:queue_cmd(Framebuffer,5658),
  rec(5658).

%% @doc glBindFramebuffer
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec bindFramebuffer(Target, Framebuffer) -> 'ok' when Target :: enum(),Framebuffer :: integer().
bindFramebuffer(Target,Framebuffer) when is_integer(Target),is_integer(Framebuffer) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Framebuffer,5659),
  ok.

%% @doc glDeleteFramebuffers
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec deleteFramebuffers(Framebuffers) -> 'ok' when Framebuffers :: [integer()].
deleteFramebuffers(Framebuffers) when is_list(Framebuffers) ->
  IF = get_interface(),
  N = length(Framebuffers),
  IF:queue_cmd(N,Framebuffers,5660),
  ok.

%% @doc glGenFramebuffers
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec genFramebuffers(N) -> [integer()] when N :: integer().
genFramebuffers(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5661),
  rec(5661).

%% @doc glCheckFramebufferStatus
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec checkFramebufferStatus(Target) -> enum() when Target :: enum().
checkFramebufferStatus(Target) when is_integer(Target) ->
  IF = get_interface(),
  IF:queue_cmd(Target,5662),
  rec(5662).

%% @doc glFramebufferTexture
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec framebufferTexture1D(Target, Attachment, Textarget, Texture, Level) -> 'ok' when Target :: enum(),Attachment :: enum(),Textarget :: enum(),Texture :: integer(),Level :: integer().
framebufferTexture1D(Target,Attachment,Textarget,Texture,Level) when is_integer(Target),is_integer(Attachment),is_integer(Textarget),is_integer(Texture),is_integer(Level) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Attachment,Textarget,Texture,Level,5663),
  ok.

%% @doc glFramebufferTexture
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec framebufferTexture2D(Target, Attachment, Textarget, Texture, Level) -> 'ok' when Target :: enum(),Attachment :: enum(),Textarget :: enum(),Texture :: integer(),Level :: integer().
framebufferTexture2D(Target,Attachment,Textarget,Texture,Level) when is_integer(Target),is_integer(Attachment),is_integer(Textarget),is_integer(Texture),is_integer(Level) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Attachment,Textarget,Texture,Level,5664),
  ok.

%% @doc glFramebufferTexture
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec framebufferTexture3D(Target, Attachment, Textarget, Texture, Level, Zoffset) -> 'ok' when Target :: enum(),Attachment :: enum(),Textarget :: enum(),Texture :: integer(),Level :: integer(),Zoffset :: integer().
framebufferTexture3D(Target,Attachment,Textarget,Texture,Level,Zoffset) when is_integer(Target),is_integer(Attachment),is_integer(Textarget),is_integer(Texture),is_integer(Level),is_integer(Zoffset) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Attachment,Textarget,Texture,Level,Zoffset,5665),
  ok.

%% @doc glFramebufferRenderbuffer
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec framebufferRenderbuffer(Target, Attachment, Renderbuffertarget, Renderbuffer) -> 'ok' when Target :: enum(),Attachment :: enum(),Renderbuffertarget :: enum(),Renderbuffer :: integer().
framebufferRenderbuffer(Target,Attachment,Renderbuffertarget,Renderbuffer) when is_integer(Target),is_integer(Attachment),is_integer(Renderbuffertarget),is_integer(Renderbuffer) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Attachment,Renderbuffertarget,Renderbuffer,5666),
  ok.

%% @doc glGetFramebufferAttachmentParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getFramebufferAttachmentParameteriv(Target, Attachment, Pname) -> integer() when Target :: enum(),Attachment :: enum(),Pname :: enum().
getFramebufferAttachmentParameteriv(Target,Attachment,Pname) when is_integer(Target),is_integer(Attachment),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Attachment,Pname,5667),
  rec(5667).

%% @doc glGenerateMipmap
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec generateMipmap(Target) -> 'ok' when Target :: enum().
generateMipmap(Target) when is_integer(Target) ->
  IF = get_interface(),
  IF:queue_cmd(Target,5668),
  ok.

%% @doc glBlitFramebuffer
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec blitFramebuffer(SrcX0, SrcY0, SrcX1, SrcY1, DstX0, DstY0, DstX1, DstY1, Mask, Filter) -> 'ok' when SrcX0 :: integer(),SrcY0 :: integer(),SrcX1 :: integer(),SrcY1 :: integer(),DstX0 :: integer(),DstY0 :: integer(),DstX1 :: integer(),DstY1 :: integer(),Mask :: integer(),Filter :: enum().
blitFramebuffer(SrcX0,SrcY0,SrcX1,SrcY1,DstX0,DstY0,DstX1,DstY1,Mask,Filter) when is_integer(SrcX0),is_integer(SrcY0),is_integer(SrcX1),is_integer(SrcY1),is_integer(DstX0),is_integer(DstY0),is_integer(DstX1),is_integer(DstY1),is_integer(Mask),is_integer(Filter) ->
  IF = get_interface(),
  IF:queue_cmd(SrcX0,SrcY0,SrcX1,SrcY1,DstX0,DstY0,DstX1,DstY1,Mask,Filter,5669),
  ok.

%% @doc glRenderbufferStorageMultisample
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec renderbufferStorageMultisample(Target, Samples, Internalformat, Width, Height) -> 'ok' when Target :: enum(),Samples :: integer(),Internalformat :: enum(),Width :: integer(),Height :: integer().
renderbufferStorageMultisample(Target,Samples,Internalformat,Width,Height) when is_integer(Target),is_integer(Samples),is_integer(Internalformat),is_integer(Width),is_integer(Height) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Samples,Internalformat,Width,Height,5670),
  ok.

%% @doc glFramebufferTexture
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec framebufferTextureLayer(Target, Attachment, Texture, Level, Layer) -> 'ok' when Target :: enum(),Attachment :: enum(),Texture :: integer(),Level :: integer(),Layer :: integer().
framebufferTextureLayer(Target,Attachment,Texture,Level,Layer) when is_integer(Target),is_integer(Attachment),is_integer(Texture),is_integer(Level),is_integer(Layer) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Attachment,Texture,Level,Layer,5671),
  ok.

%% @doc glFramebufferTexture
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec framebufferTextureFaceARB(Target, Attachment, Texture, Level, Face) -> 'ok' when Target :: enum(),Attachment :: enum(),Texture :: integer(),Level :: integer(),Face :: enum().
framebufferTextureFaceARB(Target,Attachment,Texture,Level,Face) when is_integer(Target),is_integer(Attachment),is_integer(Texture),is_integer(Level),is_integer(Face) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Attachment,Texture,Level,Face,5672),
  ok.

%% @doc glFlushMappedBufferRange
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec flushMappedBufferRange(Target, Offset, Length) -> 'ok' when Target :: enum(),Offset :: integer(),Length :: integer().
flushMappedBufferRange(Target,Offset,Length) when is_integer(Target),is_integer(Offset),is_integer(Length) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Offset,Length,5673),
  ok.

%% @doc glBindVertexArray
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec bindVertexArray(Array) -> 'ok' when Array :: integer().
bindVertexArray(Array) when is_integer(Array) ->
  IF = get_interface(),
  IF:queue_cmd(Array,5674),
  ok.

%% @doc glDeleteVertexArrays
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec deleteVertexArrays(Arrays) -> 'ok' when Arrays :: [integer()].
deleteVertexArrays(Arrays) when is_list(Arrays) ->
  IF = get_interface(),
  N = length(Arrays),
  IF:queue_cmd(N,Arrays,5675),
  ok.

%% @doc glGenVertexArrays
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec genVertexArrays(N) -> [integer()] when N :: integer().
genVertexArrays(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5676),
  rec(5676).

%% @doc glIsVertexArray
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec isVertexArray(Array) -> 0|1 when Array :: integer().
isVertexArray(Array) when is_integer(Array) ->
  IF = get_interface(),
  IF:queue_cmd(Array,5677),
  rec(5677).

%% @doc glGetUniformIndices
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getUniformIndices(Program, UniformNames) -> [integer()] when Program :: integer(),UniformNames :: iolist().
getUniformIndices(Program,UniformNames) when is_integer(Program),is_list(UniformNames) ->
  IF = get_interface(),
  UniformNamesTemp = unicode:characters_to_binary([[Str|[0]] || Str <- UniformNames ]),
  UniformCount = length(UniformNames),
  IF:queue_cmd(Program,UniformCount,UniformNamesTemp,5678),
  rec(5678).

%% @doc glGetActiveUniforms
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getActiveUniformsiv(Program, UniformIndices, Pname) -> [integer()] when Program :: integer(),UniformIndices :: [integer()],Pname :: enum().
getActiveUniformsiv(Program,UniformIndices,Pname) when is_integer(Program),is_list(UniformIndices),is_integer(Pname) ->
  IF = get_interface(),
  UniformCount = length(UniformIndices),
  IF:queue_cmd(Program,UniformCount,UniformIndices,Pname,5679),
  rec(5679).

%% @doc glGetActiveUniformName
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getActiveUniformName(Program, UniformIndex, BufSize) -> string() when Program :: integer(),UniformIndex :: integer(),BufSize :: integer().
getActiveUniformName(Program,UniformIndex,BufSize) when is_integer(Program),is_integer(UniformIndex),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Program,UniformIndex,BufSize,5680),
  rec(5680).

%% @doc glGetUniformBlockIndex
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getUniformBlockIndex(Program, UniformBlockName) -> integer() when Program :: integer(),UniformBlockName :: string().
getUniformBlockIndex(Program,UniformBlockName) when is_integer(Program),is_list(UniformBlockName) ->
  IF = get_interface(),
  UniformBlockNameBin = unicode:characters_to_binary([UniformBlockName|[0]]),
  IF:queue_cmd(Program,UniformBlockNameBin,5681),
  rec(5681).

%% @doc glGetActiveUniformBlock
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getActiveUniformBlockiv(Program, UniformBlockIndex, Pname, Params) -> 'ok' when Program :: integer(),UniformBlockIndex :: integer(),Pname :: enum(),Params :: mem().
getActiveUniformBlockiv(Program,UniformBlockIndex,Pname,Params) when is_integer(Program),is_integer(UniformBlockIndex),is_integer(Pname),is_tuple(Params) orelse is_binary(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Program,UniformBlockIndex,Pname,Params,5682),
  rec(5682).

%% @doc glGetActiveUniformBlockName
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getActiveUniformBlockName(Program, UniformBlockIndex, BufSize) -> string() when Program :: integer(),UniformBlockIndex :: integer(),BufSize :: integer().
getActiveUniformBlockName(Program,UniformBlockIndex,BufSize) when is_integer(Program),is_integer(UniformBlockIndex),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Program,UniformBlockIndex,BufSize,5683),
  rec(5683).

%% @doc glUniformBlockBinding
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniformBlockBinding(Program, UniformBlockIndex, UniformBlockBinding) -> 'ok' when Program :: integer(),UniformBlockIndex :: integer(),UniformBlockBinding :: integer().
uniformBlockBinding(Program,UniformBlockIndex,UniformBlockBinding) when is_integer(Program),is_integer(UniformBlockIndex),is_integer(UniformBlockBinding) ->
  IF = get_interface(),
  IF:queue_cmd(Program,UniformBlockIndex,UniformBlockBinding,5684),
  ok.

%% @doc glCopyBufferSubData
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec copyBufferSubData(ReadTarget, WriteTarget, ReadOffset, WriteOffset, Size) -> 'ok' when ReadTarget :: enum(),WriteTarget :: enum(),ReadOffset :: integer(),WriteOffset :: integer(),Size :: integer().
copyBufferSubData(ReadTarget,WriteTarget,ReadOffset,WriteOffset,Size) when is_integer(ReadTarget),is_integer(WriteTarget),is_integer(ReadOffset),is_integer(WriteOffset),is_integer(Size) ->
  IF = get_interface(),
  IF:queue_cmd(ReadTarget,WriteTarget,ReadOffset,WriteOffset,Size,5685),
  ok.

%% @doc glDrawElementsBaseVertex
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec drawElementsBaseVertex(Mode, Count, Type, Indices, Basevertex) -> 'ok' when Mode :: enum(),Count :: integer(),Type :: enum(),Indices :: offset()|mem(),Basevertex :: integer().
drawElementsBaseVertex(Mode,Count,Type,Indices,Basevertex) when is_integer(Mode),is_integer(Count),is_integer(Type),is_integer(Indices) orelse is_tuple(Indices) orelse is_binary(Indices),is_integer(Basevertex) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Count,Type,Indices,Basevertex,5686),
  ok.

%% @doc glDrawRangeElementsBaseVertex
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec drawRangeElementsBaseVertex(Mode, Start, End, Count, Type, Indices, Basevertex) -> 'ok' when Mode :: enum(),Start :: integer(),End :: integer(),Count :: integer(),Type :: enum(),Indices :: offset()|mem(),Basevertex :: integer().
drawRangeElementsBaseVertex(Mode,Start,End,Count,Type,Indices,Basevertex) when is_integer(Mode),is_integer(Start),is_integer(End),is_integer(Count),is_integer(Type),is_integer(Indices) orelse is_tuple(Indices) orelse is_binary(Indices),is_integer(Basevertex) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Start,End,Count,Type,Indices,Basevertex,5688),
  ok.

%% @doc glDrawElementsInstancedBaseVertex
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec drawElementsInstancedBaseVertex(Mode, Count, Type, Indices, Primcount, Basevertex) -> 'ok' when Mode :: enum(),Count :: integer(),Type :: enum(),Indices :: offset()|mem(),Primcount :: integer(),Basevertex :: integer().
drawElementsInstancedBaseVertex(Mode,Count,Type,Indices,Primcount,Basevertex) when is_integer(Mode),is_integer(Count),is_integer(Type),is_integer(Indices) orelse is_tuple(Indices) orelse is_binary(Indices),is_integer(Primcount),is_integer(Basevertex) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Count,Type,Indices,Primcount,Basevertex,5690),
  ok.

%% @doc glProvokingVertex
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec provokingVertex(Mode) -> 'ok' when Mode :: enum().
provokingVertex(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5692),
  ok.

%% @doc glFenceSync
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec fenceSync(Condition, Flags) -> integer() when Condition :: enum(),Flags :: integer().
fenceSync(Condition,Flags) when is_integer(Condition),is_integer(Flags) ->
  IF = get_interface(),
  IF:queue_cmd(Condition,Flags,5693),
  rec(5693).

%% @doc glIsSync
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec isSync(Sync) -> 0|1 when Sync :: integer().
isSync(Sync) when is_integer(Sync) ->
  IF = get_interface(),
  IF:queue_cmd(Sync,5694),
  rec(5694).

%% @doc glDeleteSync
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec deleteSync(Sync) -> 'ok' when Sync :: integer().
deleteSync(Sync) when is_integer(Sync) ->
  IF = get_interface(),
  IF:queue_cmd(Sync,5695),
  ok.

%% @doc glClientWaitSync
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec clientWaitSync(Sync, Flags, Timeout) -> enum() when Sync :: integer(),Flags :: integer(),Timeout :: integer().
clientWaitSync(Sync,Flags,Timeout) when is_integer(Sync),is_integer(Flags),is_integer(Timeout) ->
  IF = get_interface(),
  IF:queue_cmd(Sync,Flags,Timeout,5696),
  rec(5696).

%% @doc glWaitSync
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec waitSync(Sync, Flags, Timeout) -> 'ok' when Sync :: integer(),Flags :: integer(),Timeout :: integer().
waitSync(Sync,Flags,Timeout) when is_integer(Sync),is_integer(Flags),is_integer(Timeout) ->
  IF = get_interface(),
  IF:queue_cmd(Sync,Flags,Timeout,5697),
  ok.

%% @doc glGet
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getInteger64v(Pname) -> [integer()] when Pname :: enum().
getInteger64v(Pname) when is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,5698),
  rec(5698).

%% @doc glGetSync
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getSynciv(Sync, Pname, BufSize) -> [integer()] when Sync :: integer(),Pname :: enum(),BufSize :: integer().
getSynciv(Sync,Pname,BufSize) when is_integer(Sync),is_integer(Pname),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Sync,Pname,BufSize,5699),
  rec(5699).

%% @doc glTexImage2DMultisample
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texImage2DMultisample(Target, Samples, Internalformat, Width, Height, Fixedsamplelocations) -> 'ok' when Target :: enum(),Samples :: integer(),Internalformat :: integer(),Width :: integer(),Height :: integer(),Fixedsamplelocations :: 0|1.
texImage2DMultisample(Target,Samples,Internalformat,Width,Height,Fixedsamplelocations) when is_integer(Target),is_integer(Samples),is_integer(Internalformat),is_integer(Width),is_integer(Height),(0 =:= Fixedsamplelocations) orelse (1 =:= Fixedsamplelocations) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Samples,Internalformat,Width,Height,Fixedsamplelocations,5700),
  ok.

%% @doc glTexImage3DMultisample
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texImage3DMultisample(Target, Samples, Internalformat, Width, Height, Depth, Fixedsamplelocations) -> 'ok' when Target :: enum(),Samples :: integer(),Internalformat :: integer(),Width :: integer(),Height :: integer(),Depth :: integer(),Fixedsamplelocations :: 0|1.
texImage3DMultisample(Target,Samples,Internalformat,Width,Height,Depth,Fixedsamplelocations) when is_integer(Target),is_integer(Samples),is_integer(Internalformat),is_integer(Width),is_integer(Height),is_integer(Depth),(0 =:= Fixedsamplelocations) orelse (1 =:= Fixedsamplelocations) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Samples,Internalformat,Width,Height,Depth,Fixedsamplelocations,5701),
  ok.

%% @doc glGetMultisample
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getMultisamplefv(Pname, Index) -> {float(),float()} when Pname :: enum(),Index :: integer().
getMultisamplefv(Pname,Index) when is_integer(Pname),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Index,5702),
  rec(5702).

%% @doc glSampleMaski
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec sampleMaski(Index, Mask) -> 'ok' when Index :: integer(),Mask :: integer().
sampleMaski(Index,Mask) when is_integer(Index),is_integer(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Mask,5703),
  ok.

%% @doc glNamedStringARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec namedStringARB(Type, Name, String) -> 'ok' when Type :: enum(),Name :: string(),String :: string().
namedStringARB(Type,Name,String) when is_integer(Type),is_list(Name),is_list(String) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  StringBin = unicode:characters_to_binary([String|[0]]),
  IF:queue_cmd(Type,NameBin,StringBin,5704),
  ok.

%% @doc glDeleteNamedStringARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec deleteNamedStringARB(Name) -> 'ok' when Name :: string().
deleteNamedStringARB(Name) when is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(NameBin,5705),
  ok.

%% @doc glCompileShaderIncludeARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec compileShaderIncludeARB(Shader, Path) -> 'ok' when Shader :: integer(),Path :: iolist().
compileShaderIncludeARB(Shader,Path) when is_integer(Shader),is_list(Path) ->
  IF = get_interface(),
  PathTemp = unicode:characters_to_binary([[Str|[0]] || Str <- Path ]),
  Count = length(Path),
  IF:queue_cmd(Shader,Count,PathTemp,5706),
  ok.

%% @doc glIsNamedStringARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec isNamedStringARB(Name) -> 0|1 when Name :: string().
isNamedStringARB(Name) when is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(NameBin,5707),
  rec(5707).

%% @doc glGetNamedStringARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getNamedStringARB(Name, BufSize) -> string() when Name :: string(),BufSize :: integer().
getNamedStringARB(Name,BufSize) when is_list(Name),is_integer(BufSize) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(NameBin,BufSize,5708),
  rec(5708).

%% @doc glGetNamedStringARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getNamedStringivARB(Name, Pname) -> integer() when Name :: string(),Pname :: enum().
getNamedStringivARB(Name,Pname) when is_list(Name),is_integer(Pname) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(NameBin,Pname,5709),
  rec(5709).

%% @doc glBindFragDataLocationIndexe
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec bindFragDataLocationIndexed(Program, ColorNumber, Index, Name) -> 'ok' when Program :: integer(),ColorNumber :: integer(),Index :: integer(),Name :: string().
bindFragDataLocationIndexed(Program,ColorNumber,Index,Name) when is_integer(Program),is_integer(ColorNumber),is_integer(Index),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,ColorNumber,Index,NameBin,5710),
  ok.

%% @doc glGetFragDataIndex
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getFragDataIndex(Program, Name) -> integer() when Program :: integer(),Name :: string().
getFragDataIndex(Program,Name) when is_integer(Program),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,NameBin,5711),
  rec(5711).

%% @doc glGenSamplers
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec genSamplers(Count) -> [integer()] when Count :: integer().
genSamplers(Count) when is_integer(Count) ->
  IF = get_interface(),
  IF:queue_cmd(Count,5712),
  rec(5712).

%% @doc glDeleteSamplers
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec deleteSamplers(Samplers) -> 'ok' when Samplers :: [integer()].
deleteSamplers(Samplers) when is_list(Samplers) ->
  IF = get_interface(),
  Count = length(Samplers),
  IF:queue_cmd(Count,Samplers,5713),
  ok.

%% @doc glIsSampler
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec isSampler(Sampler) -> 0|1 when Sampler :: integer().
isSampler(Sampler) when is_integer(Sampler) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,5714),
  rec(5714).

%% @doc glBindSampler
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec bindSampler(Unit, Sampler) -> 'ok' when Unit :: integer(),Sampler :: integer().
bindSampler(Unit,Sampler) when is_integer(Unit),is_integer(Sampler) ->
  IF = get_interface(),
  IF:queue_cmd(Unit,Sampler,5715),
  ok.

%% @doc glSamplerParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec samplerParameteri(Sampler, Pname, Param) -> 'ok' when Sampler :: integer(),Pname :: enum(),Param :: integer().
samplerParameteri(Sampler,Pname,Param) when is_integer(Sampler),is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,Param,5716),
  ok.

%% @doc glSamplerParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec samplerParameteriv(Sampler, Pname, Param) -> 'ok' when Sampler :: integer(),Pname :: enum(),Param :: [integer()].
samplerParameteriv(Sampler,Pname,Param) when is_integer(Sampler),is_integer(Pname),is_list(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,Param,5717),
  ok.

%% @doc glSamplerParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec samplerParameterf(Sampler, Pname, Param) -> 'ok' when Sampler :: integer(),Pname :: enum(),Param :: float().
samplerParameterf(Sampler,Pname,Param) when is_integer(Sampler),is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,Param,5718),
  ok.

%% @doc glSamplerParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec samplerParameterfv(Sampler, Pname, Param) -> 'ok' when Sampler :: integer(),Pname :: enum(),Param :: [float()].
samplerParameterfv(Sampler,Pname,Param) when is_integer(Sampler),is_integer(Pname),is_list(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,Param,5719),
  ok.

%% @doc glSamplerParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec samplerParameterIiv(Sampler, Pname, Param) -> 'ok' when Sampler :: integer(),Pname :: enum(),Param :: [integer()].
samplerParameterIiv(Sampler,Pname,Param) when is_integer(Sampler),is_integer(Pname),is_list(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,Param,5720),
  ok.

%% @doc glSamplerParameterI
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec samplerParameterIuiv(Sampler, Pname, Param) -> 'ok' when Sampler :: integer(),Pname :: enum(),Param :: [integer()].
samplerParameterIuiv(Sampler,Pname,Param) when is_integer(Sampler),is_integer(Pname),is_list(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,Param,5721),
  ok.

%% @doc glGetSamplerParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getSamplerParameteriv(Sampler, Pname) -> [integer()] when Sampler :: integer(),Pname :: enum().
getSamplerParameteriv(Sampler,Pname) when is_integer(Sampler),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,5722),
  rec(5722).

%% @doc glGetSamplerParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getSamplerParameterIiv(Sampler, Pname) -> [integer()] when Sampler :: integer(),Pname :: enum().
getSamplerParameterIiv(Sampler,Pname) when is_integer(Sampler),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,5723),
  rec(5723).

%% @doc glGetSamplerParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getSamplerParameterfv(Sampler, Pname) -> [float()] when Sampler :: integer(),Pname :: enum().
getSamplerParameterfv(Sampler,Pname) when is_integer(Sampler),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,5724),
  rec(5724).

%% @doc glGetSamplerParameterI
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getSamplerParameterIuiv(Sampler, Pname) -> [integer()] when Sampler :: integer(),Pname :: enum().
getSamplerParameterIuiv(Sampler,Pname) when is_integer(Sampler),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,5725),
  rec(5725).

%% @doc glQueryCounter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec queryCounter(Id, Target) -> 'ok' when Id :: integer(),Target :: enum().
queryCounter(Id,Target) when is_integer(Id),is_integer(Target) ->
  IF = get_interface(),
  IF:queue_cmd(Id,Target,5726),
  ok.

%% @doc glGetQueryObjecti64v
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getQueryObjecti64v(Id, Pname) -> integer() when Id :: integer(),Pname :: enum().
getQueryObjecti64v(Id,Pname) when is_integer(Id),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Id,Pname,5727),
  rec(5727).

%% @doc glGetQueryObjectui64v
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getQueryObjectui64v(Id, Pname) -> integer() when Id :: integer(),Pname :: enum().
getQueryObjectui64v(Id,Pname) when is_integer(Id),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Id,Pname,5728),
  rec(5728).

%% @doc glDrawArraysIndirect
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec drawArraysIndirect(Mode, Indirect) -> 'ok' when Mode :: enum(),Indirect :: offset()|mem().
drawArraysIndirect(Mode,Indirect) when is_integer(Mode),is_integer(Indirect) orelse is_tuple(Indirect) orelse is_binary(Indirect) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Indirect,5729),
  ok.

%% @doc glDrawElementsIndirect
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec drawElementsIndirect(Mode, Type, Indirect) -> 'ok' when Mode :: enum(),Type :: enum(),Indirect :: offset()|mem().
drawElementsIndirect(Mode,Type,Indirect) when is_integer(Mode),is_integer(Type),is_integer(Indirect) orelse is_tuple(Indirect) orelse is_binary(Indirect) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Type,Indirect,5731),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniform1d(Location, X) -> 'ok' when Location :: integer(),X :: float().
uniform1d(Location,X) when is_integer(Location),is_float(X) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,5733),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniform2d(Location, X, Y) -> 'ok' when Location :: integer(),X :: float(),Y :: float().
uniform2d(Location,X,Y) when is_integer(Location),is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,Y,5734),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniform3d(Location, X, Y, Z) -> 'ok' when Location :: integer(),X :: float(),Y :: float(),Z :: float().
uniform3d(Location,X,Y,Z) when is_integer(Location),is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,Y,Z,5735),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniform4d(Location, X, Y, Z, W) -> 'ok' when Location :: integer(),X :: float(),Y :: float(),Z :: float(),W :: float().
uniform4d(Location,X,Y,Z,W) when is_integer(Location),is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,Y,Z,W,5736),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniform1dv(Location, Value) -> 'ok' when Location :: integer(),Value :: [float()].
uniform1dv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5737),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniform2dv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{float(),float()}].
uniform2dv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5738),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniform3dv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{float(),float(),float()}].
uniform3dv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5739),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniform4dv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{float(),float(),float(),float()}].
uniform4dv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5740),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniformMatrix2dv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float()}].
uniformMatrix2dv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5741),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniformMatrix3dv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix3dv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5742),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniformMatrix4dv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix4dv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5743),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniformMatrix2x3dv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float()}].
uniformMatrix2x3dv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5744),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniformMatrix2x4dv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix2x4dv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5745),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniformMatrix3x2dv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float()}].
uniformMatrix3x2dv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5746),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniformMatrix3x4dv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix3x4dv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5747),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniformMatrix4x2dv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix4x2dv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5748),
  ok.

%% @doc glUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniformMatrix4x3dv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix4x3dv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5749),
  ok.

%% @doc glGetUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getUniformdv(Program, Location) -> matrix() when Program :: integer(),Location :: integer().
getUniformdv(Program,Location) when is_integer(Program),is_integer(Location) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,5750),
  rec(5750).

%% @doc glGetSubroutineUniformLocation
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getSubroutineUniformLocation(Program, Shadertype, Name) -> integer() when Program :: integer(),Shadertype :: enum(),Name :: string().
getSubroutineUniformLocation(Program,Shadertype,Name) when is_integer(Program),is_integer(Shadertype),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,Shadertype,NameBin,5751),
  rec(5751).

%% @doc glGetSubroutineIndex
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getSubroutineIndex(Program, Shadertype, Name) -> integer() when Program :: integer(),Shadertype :: enum(),Name :: string().
getSubroutineIndex(Program,Shadertype,Name) when is_integer(Program),is_integer(Shadertype),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,Shadertype,NameBin,5752),
  rec(5752).

%% @doc glGetActiveSubroutineUniformName
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getActiveSubroutineUniformName(Program, Shadertype, Index, Bufsize) -> string() when Program :: integer(),Shadertype :: enum(),Index :: integer(),Bufsize :: integer().
getActiveSubroutineUniformName(Program,Shadertype,Index,Bufsize) when is_integer(Program),is_integer(Shadertype),is_integer(Index),is_integer(Bufsize) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Shadertype,Index,Bufsize,5753),
  rec(5753).

%% @doc glGetActiveSubroutineName
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getActiveSubroutineName(Program, Shadertype, Index, Bufsize) -> string() when Program :: integer(),Shadertype :: enum(),Index :: integer(),Bufsize :: integer().
getActiveSubroutineName(Program,Shadertype,Index,Bufsize) when is_integer(Program),is_integer(Shadertype),is_integer(Index),is_integer(Bufsize) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Shadertype,Index,Bufsize,5754),
  rec(5754).

%% @doc glUniformSubroutines
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec uniformSubroutinesuiv(Shadertype, Indices) -> 'ok' when Shadertype :: enum(),Indices :: [integer()].
uniformSubroutinesuiv(Shadertype,Indices) when is_integer(Shadertype),is_list(Indices) ->
  IF = get_interface(),
  Count = length(Indices),
  IF:queue_cmd(Shadertype,Count,Indices,5755),
  ok.

%% @doc glGetUniformSubroutine
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getUniformSubroutineuiv(Shadertype, Location) -> {integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer()} when Shadertype :: enum(),Location :: integer().
getUniformSubroutineuiv(Shadertype,Location) when is_integer(Shadertype),is_integer(Location) ->
  IF = get_interface(),
  IF:queue_cmd(Shadertype,Location,5756),
  rec(5756).

%% @doc glGetProgramStage
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getProgramStageiv(Program, Shadertype, Pname) -> integer() when Program :: integer(),Shadertype :: enum(),Pname :: enum().
getProgramStageiv(Program,Shadertype,Pname) when is_integer(Program),is_integer(Shadertype),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Shadertype,Pname,5757),
  rec(5757).

%% @doc glPatchParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec patchParameteri(Pname, Value) -> 'ok' when Pname :: enum(),Value :: integer().
patchParameteri(Pname,Value) when is_integer(Pname),is_integer(Value) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Value,5758),
  ok.

%% @doc glPatchParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec patchParameterfv(Pname, Values) -> 'ok' when Pname :: enum(),Values :: [float()].
patchParameterfv(Pname,Values) when is_integer(Pname),is_list(Values) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Values,5759),
  ok.

%% @doc glBindTransformFeedback
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec bindTransformFeedback(Target, Id) -> 'ok' when Target :: enum(),Id :: integer().
bindTransformFeedback(Target,Id) when is_integer(Target),is_integer(Id) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Id,5760),
  ok.

%% @doc glDeleteTransformFeedbacks
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec deleteTransformFeedbacks(Ids) -> 'ok' when Ids :: [integer()].
deleteTransformFeedbacks(Ids) when is_list(Ids) ->
  IF = get_interface(),
  N = length(Ids),
  IF:queue_cmd(N,Ids,5761),
  ok.

%% @doc glGenTransformFeedbacks
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec genTransformFeedbacks(N) -> [integer()] when N :: integer().
genTransformFeedbacks(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5762),
  rec(5762).

%% @doc glIsTransformFeedback
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec isTransformFeedback(Id) -> 0|1 when Id :: integer().
isTransformFeedback(Id) when is_integer(Id) ->
  IF = get_interface(),
  IF:queue_cmd(Id,5763),
  rec(5763).

%% @doc glPauseTransformFeedback
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec pauseTransformFeedback() -> 'ok'.
pauseTransformFeedback()  ->
  IF = get_interface(),
  IF:queue_cmd(5764),
  ok.

%% @doc glResumeTransformFeedback
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec resumeTransformFeedback() -> 'ok'.
resumeTransformFeedback()  ->
  IF = get_interface(),
  IF:queue_cmd(5765),
  ok.

%% @doc glDrawTransformFeedback
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec drawTransformFeedback(Mode, Id) -> 'ok' when Mode :: enum(),Id :: integer().
drawTransformFeedback(Mode,Id) when is_integer(Mode),is_integer(Id) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Id,5766),
  ok.

%% @doc glDrawTransformFeedbackStream
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec drawTransformFeedbackStream(Mode, Id, Stream) -> 'ok' when Mode :: enum(),Id :: integer(),Stream :: integer().
drawTransformFeedbackStream(Mode,Id,Stream) when is_integer(Mode),is_integer(Id),is_integer(Stream) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Id,Stream,5767),
  ok.

%% @doc glBeginQueryIndexe
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec beginQueryIndexed(Target, Index, Id) -> 'ok' when Target :: enum(),Index :: integer(),Id :: integer().
beginQueryIndexed(Target,Index,Id) when is_integer(Target),is_integer(Index),is_integer(Id) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,Id,5768),
  ok.

%% @doc glBeginQueryIndexed
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec endQueryIndexed(Target, Index) -> 'ok' when Target :: enum(),Index :: integer().
endQueryIndexed(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5769),
  ok.

%% @doc glGetQueryIndexed
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getQueryIndexediv(Target, Index, Pname) -> integer() when Target :: enum(),Index :: integer(),Pname :: enum().
getQueryIndexediv(Target,Index,Pname) when is_integer(Target),is_integer(Index),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,Pname,5770),
  rec(5770).

%% @doc glReleaseShaderCompiler
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec releaseShaderCompiler() -> 'ok'.
releaseShaderCompiler()  ->
  IF = get_interface(),
  IF:queue_cmd(5771),
  ok.

%% @doc glShaderBinary
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec shaderBinary(Shaders, Binaryformat, Binary) -> 'ok' when Shaders :: [integer()],Binaryformat :: enum(),Binary :: binary().
shaderBinary(Shaders,Binaryformat,Binary) when is_list(Shaders),is_integer(Binaryformat),is_binary(Binary) ->
  IF = get_interface(),
  Count = length(Shaders),
  IF:queue_cmd(Count,Shaders,Binaryformat,Binary,5772),
  ok.

%% @doc glGetShaderPrecisionFormat
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getShaderPrecisionFormat(Shadertype, Precisiontype) -> {Range :: {integer(),integer()},Precision :: integer()} when Shadertype :: enum(),Precisiontype :: enum().
getShaderPrecisionFormat(Shadertype,Precisiontype) when is_integer(Shadertype),is_integer(Precisiontype) ->
  IF = get_interface(),
  IF:queue_cmd(Shadertype,Precisiontype,5773),
  rec(5773).

%% @doc glDepthRange
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec depthRangef(N, F) -> 'ok' when N :: clamp(),F :: clamp().
depthRangef(N,F) when is_float(N),is_float(F) ->
  IF = get_interface(),
  IF:queue_cmd(N,F,5774),
  ok.

%% @doc glClearDepthf
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec clearDepthf(D) -> 'ok' when D :: clamp().
clearDepthf(D) when is_float(D) ->
  IF = get_interface(),
  IF:queue_cmd(D,5775),
  ok.

%% @doc glGetProgramBinary
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getProgramBinary(Program, BufSize) -> {BinaryFormat :: enum(),Binary :: binary()} when Program :: integer(),BufSize :: integer().
getProgramBinary(Program,BufSize) when is_integer(Program),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Program,BufSize,5776),
  rec(5776).

%% @doc glProgramBinary
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programBinary(Program, BinaryFormat, Binary) -> 'ok' when Program :: integer(),BinaryFormat :: enum(),Binary :: binary().
programBinary(Program,BinaryFormat,Binary) when is_integer(Program),is_integer(BinaryFormat),is_binary(Binary) ->
  IF = get_interface(),
  IF:queue_cmd(Program,BinaryFormat,Binary,5777),
  ok.

%% @doc glProgramParameter
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programParameteri(Program, Pname, Value) -> 'ok' when Program :: integer(),Pname :: enum(),Value :: integer().
programParameteri(Program,Pname,Value) when is_integer(Program),is_integer(Pname),is_integer(Value) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Pname,Value,5778),
  ok.

%% @doc glUseProgramStages
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec useProgramStages(Pipeline, Stages, Program) -> 'ok' when Pipeline :: integer(),Stages :: integer(),Program :: integer().
useProgramStages(Pipeline,Stages,Program) when is_integer(Pipeline),is_integer(Stages),is_integer(Program) ->
  IF = get_interface(),
  IF:queue_cmd(Pipeline,Stages,Program,5779),
  ok.

%% @doc glActiveShaderProgram
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec activeShaderProgram(Pipeline, Program) -> 'ok' when Pipeline :: integer(),Program :: integer().
activeShaderProgram(Pipeline,Program) when is_integer(Pipeline),is_integer(Program) ->
  IF = get_interface(),
  IF:queue_cmd(Pipeline,Program,5780),
  ok.

%% @doc glCreateShaderProgramv
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec createShaderProgramv(Type, Strings) -> integer() when Type :: enum(),Strings :: iolist().
createShaderProgramv(Type,Strings) when is_integer(Type),is_list(Strings) ->
  IF = get_interface(),
  StringsTemp = unicode:characters_to_binary([[Str|[0]] || Str <- Strings ]),
  Count = length(Strings),
  IF:queue_cmd(Type,Count,StringsTemp,5781),
  rec(5781).

%% @doc glBindProgramPipeline
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec bindProgramPipeline(Pipeline) -> 'ok' when Pipeline :: integer().
bindProgramPipeline(Pipeline) when is_integer(Pipeline) ->
  IF = get_interface(),
  IF:queue_cmd(Pipeline,5782),
  ok.

%% @doc glDeleteProgramPipelines
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec deleteProgramPipelines(Pipelines) -> 'ok' when Pipelines :: [integer()].
deleteProgramPipelines(Pipelines) when is_list(Pipelines) ->
  IF = get_interface(),
  N = length(Pipelines),
  IF:queue_cmd(N,Pipelines,5783),
  ok.

%% @doc glGenProgramPipelines
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec genProgramPipelines(N) -> [integer()] when N :: integer().
genProgramPipelines(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5784),
  rec(5784).

%% @doc glIsProgramPipeline
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec isProgramPipeline(Pipeline) -> 0|1 when Pipeline :: integer().
isProgramPipeline(Pipeline) when is_integer(Pipeline) ->
  IF = get_interface(),
  IF:queue_cmd(Pipeline,5785),
  rec(5785).

%% @doc glGetProgramPipeline
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getProgramPipelineiv(Pipeline, Pname) -> integer() when Pipeline :: integer(),Pname :: enum().
getProgramPipelineiv(Pipeline,Pname) when is_integer(Pipeline),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Pipeline,Pname,5786),
  rec(5786).

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniform1i(Program, Location, V0) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: integer().
programUniform1i(Program,Location,V0) when is_integer(Program),is_integer(Location),is_integer(V0) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,5787),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniform1iv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [integer()].
programUniform1iv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5788),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniform1f(Program, Location, V0) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: float().
programUniform1f(Program,Location,V0) when is_integer(Program),is_integer(Location),is_float(V0) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,5789),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniform1fv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [float()].
programUniform1fv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5790),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniform1d(Program, Location, V0) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: float().
programUniform1d(Program,Location,V0) when is_integer(Program),is_integer(Location),is_float(V0) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,5791),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniform1dv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [float()].
programUniform1dv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5792),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniform1ui(Program, Location, V0) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: integer().
programUniform1ui(Program,Location,V0) when is_integer(Program),is_integer(Location),is_integer(V0) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,5793),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniform1uiv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [integer()].
programUniform1uiv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5794),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniform2i(Program, Location, V0, V1) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: integer(),V1 :: integer().
programUniform2i(Program,Location,V0,V1) when is_integer(Program),is_integer(Location),is_integer(V0),is_integer(V1) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,5795),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniform2iv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{integer(),integer()}].
programUniform2iv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5796),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniform2f(Program, Location, V0, V1) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: float(),V1 :: float().
programUniform2f(Program,Location,V0,V1) when is_integer(Program),is_integer(Location),is_float(V0),is_float(V1) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,5797),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniform2fv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{float(),float()}].
programUniform2fv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5798),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniform2d(Program, Location, V0, V1) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: float(),V1 :: float().
programUniform2d(Program,Location,V0,V1) when is_integer(Program),is_integer(Location),is_float(V0),is_float(V1) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,5799),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniform2dv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{float(),float()}].
programUniform2dv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5800),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniform2ui(Program, Location, V0, V1) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: integer(),V1 :: integer().
programUniform2ui(Program,Location,V0,V1) when is_integer(Program),is_integer(Location),is_integer(V0),is_integer(V1) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,5801),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniform2uiv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{integer(),integer()}].
programUniform2uiv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5802),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniform3i(Program, Location, V0, V1, V2) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: integer(),V1 :: integer(),V2 :: integer().
programUniform3i(Program,Location,V0,V1,V2) when is_integer(Program),is_integer(Location),is_integer(V0),is_integer(V1),is_integer(V2) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,V2,5803),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniform3iv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{integer(),integer(),integer()}].
programUniform3iv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5804),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniform3f(Program, Location, V0, V1, V2) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: float(),V1 :: float(),V2 :: float().
programUniform3f(Program,Location,V0,V1,V2) when is_integer(Program),is_integer(Location),is_float(V0),is_float(V1),is_float(V2) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,V2,5805),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniform3fv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{float(),float(),float()}].
programUniform3fv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5806),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniform3d(Program, Location, V0, V1, V2) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: float(),V1 :: float(),V2 :: float().
programUniform3d(Program,Location,V0,V1,V2) when is_integer(Program),is_integer(Location),is_float(V0),is_float(V1),is_float(V2) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,V2,5807),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniform3dv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{float(),float(),float()}].
programUniform3dv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5808),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniform3ui(Program, Location, V0, V1, V2) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: integer(),V1 :: integer(),V2 :: integer().
programUniform3ui(Program,Location,V0,V1,V2) when is_integer(Program),is_integer(Location),is_integer(V0),is_integer(V1),is_integer(V2) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,V2,5809),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniform3uiv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{integer(),integer(),integer()}].
programUniform3uiv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5810),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniform4i(Program, Location, V0, V1, V2, V3) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: integer(),V1 :: integer(),V2 :: integer(),V3 :: integer().
programUniform4i(Program,Location,V0,V1,V2,V3) when is_integer(Program),is_integer(Location),is_integer(V0),is_integer(V1),is_integer(V2),is_integer(V3) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,V2,V3,5811),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniform4iv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{integer(),integer(),integer(),integer()}].
programUniform4iv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5812),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniform4f(Program, Location, V0, V1, V2, V3) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: float(),V1 :: float(),V2 :: float(),V3 :: float().
programUniform4f(Program,Location,V0,V1,V2,V3) when is_integer(Program),is_integer(Location),is_float(V0),is_float(V1),is_float(V2),is_float(V3) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,V2,V3,5813),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniform4fv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{float(),float(),float(),float()}].
programUniform4fv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5814),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniform4d(Program, Location, V0, V1, V2, V3) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: float(),V1 :: float(),V2 :: float(),V3 :: float().
programUniform4d(Program,Location,V0,V1,V2,V3) when is_integer(Program),is_integer(Location),is_float(V0),is_float(V1),is_float(V2),is_float(V3) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,V2,V3,5815),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniform4dv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{float(),float(),float(),float()}].
programUniform4dv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5816),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniform4ui(Program, Location, V0, V1, V2, V3) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: integer(),V1 :: integer(),V2 :: integer(),V3 :: integer().
programUniform4ui(Program,Location,V0,V1,V2,V3) when is_integer(Program),is_integer(Location),is_integer(V0),is_integer(V1),is_integer(V2),is_integer(V3) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,V2,V3,5817),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniform4uiv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{integer(),integer(),integer(),integer()}].
programUniform4uiv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5818),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniformMatrix2fv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float()}].
programUniformMatrix2fv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5819),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniformMatrix3fv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix3fv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5820),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniformMatrix4fv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix4fv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5821),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniformMatrix2dv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float()}].
programUniformMatrix2dv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5822),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniformMatrix3dv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix3dv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5823),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniformMatrix4dv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix4dv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5824),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniformMatrix2x3fv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float()}].
programUniformMatrix2x3fv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5825),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniformMatrix3x2fv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float()}].
programUniformMatrix3x2fv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5826),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniformMatrix2x4fv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix2x4fv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5827),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniformMatrix4x2fv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix4x2fv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5828),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniformMatrix3x4fv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix3x4fv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5829),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniformMatrix4x3fv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix4x3fv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5830),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniformMatrix2x3dv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float()}].
programUniformMatrix2x3dv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5831),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniformMatrix3x2dv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float()}].
programUniformMatrix3x2dv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5832),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniformMatrix2x4dv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix2x4dv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5833),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniformMatrix4x2dv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix4x2dv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5834),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniformMatrix3x4dv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix3x4dv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5835),
  ok.

%% @doc glProgramUniform
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programUniformMatrix4x3dv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix4x3dv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5836),
  ok.

%% @doc glValidateProgramPipeline
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec validateProgramPipeline(Pipeline) -> 'ok' when Pipeline :: integer().
validateProgramPipeline(Pipeline) when is_integer(Pipeline) ->
  IF = get_interface(),
  IF:queue_cmd(Pipeline,5837),
  ok.

%% @doc glGetProgramPipelineInfoLog
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getProgramPipelineInfoLog(Pipeline, BufSize) -> string() when Pipeline :: integer(),BufSize :: integer().
getProgramPipelineInfoLog(Pipeline,BufSize) when is_integer(Pipeline),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Pipeline,BufSize,5838),
  rec(5838).

%% @doc glVertexAttribL
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttribL1d(Index, X) -> 'ok' when Index :: integer(),X :: float().
vertexAttribL1d(Index,X) when is_integer(Index),is_float(X) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,5839),
  ok.

%% @doc glVertexAttribL
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttribL2d(Index, X, Y) -> 'ok' when Index :: integer(),X :: float(),Y :: float().
vertexAttribL2d(Index,X,Y) when is_integer(Index),is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,5840),
  ok.

%% @doc glVertexAttribL
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttribL3d(Index, X, Y, Z) -> 'ok' when Index :: integer(),X :: float(),Y :: float(),Z :: float().
vertexAttribL3d(Index,X,Y,Z) when is_integer(Index),is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,5841),
  ok.

%% @doc glVertexAttribL
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttribL4d(Index, X, Y, Z, W) -> 'ok' when Index :: integer(),X :: float(),Y :: float(),Z :: float(),W :: float().
vertexAttribL4d(Index,X,Y,Z,W) when is_integer(Index),is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,W,5842),
  ok.

%% @equiv vertexAttribL1d(Index,X)
-spec vertexAttribL1dv(Index :: integer(),V) -> 'ok' when V :: {X :: float()}.
vertexAttribL1dv(Index,{X}) ->  vertexAttribL1d(Index,X).
%% @equiv vertexAttribL2d(Index,X,Y)
-spec vertexAttribL2dv(Index :: integer(),V) -> 'ok' when V :: {X :: float(),Y :: float()}.
vertexAttribL2dv(Index,{X,Y}) ->  vertexAttribL2d(Index,X,Y).
%% @equiv vertexAttribL3d(Index,X,Y,Z)
-spec vertexAttribL3dv(Index :: integer(),V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float()}.
vertexAttribL3dv(Index,{X,Y,Z}) ->  vertexAttribL3d(Index,X,Y,Z).
%% @equiv vertexAttribL4d(Index,X,Y,Z,W)
-spec vertexAttribL4dv(Index :: integer(),V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float(),W :: float()}.
vertexAttribL4dv(Index,{X,Y,Z,W}) ->  vertexAttribL4d(Index,X,Y,Z,W).
%% @doc glVertexAttribLPointer
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttribLPointer(Index, Size, Type, Stride, Pointer) -> 'ok' when Index :: integer(),Size :: integer(),Type :: enum(),Stride :: integer(),Pointer :: offset()|mem().
vertexAttribLPointer(Index,Size,Type,Stride,Pointer) when is_integer(Index),is_integer(Size),is_integer(Type),is_integer(Stride),is_integer(Pointer) orelse is_tuple(Pointer) orelse is_binary(Pointer) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Size,Type,Stride,Pointer,5843),
  ok.

%% @doc glGetVertexAttribL
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getVertexAttribLdv(Index, Pname) -> {float(),float(),float(),float()} when Index :: integer(),Pname :: enum().
getVertexAttribLdv(Index,Pname) when is_integer(Index),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Pname,5845),
  rec(5845).

%% @doc glViewportArrayv
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec viewportArrayv(First, V) -> 'ok' when First :: integer(),V :: [{float(),float(),float(),float()}].
viewportArrayv(First,V) when is_integer(First),is_list(V) ->
  IF = get_interface(),
  Count = length(V),
  IF:queue_cmd(First,Count,V,5846),
  ok.

%% @doc glViewportIndexed
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec viewportIndexedf(Index, X, Y, W, H) -> 'ok' when Index :: integer(),X :: float(),Y :: float(),W :: float(),H :: float().
viewportIndexedf(Index,X,Y,W,H) when is_integer(Index),is_float(X),is_float(Y),is_float(W),is_float(H) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,W,H,5847),
  ok.

%% @doc glViewportIndexed
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec viewportIndexedfv(Index, V) -> 'ok' when Index :: integer(),V :: {float(),float(),float(),float()}.
viewportIndexedfv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5848),
  ok.

%% @doc glScissorArrayv
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec scissorArrayv(First, V) -> 'ok' when First :: integer(),V :: [{integer(),integer(),integer(),integer()}].
scissorArrayv(First,V) when is_integer(First),is_list(V) ->
  IF = get_interface(),
  Count = length(V),
  IF:queue_cmd(First,Count,V,5849),
  ok.

%% @doc glScissorIndexe
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec scissorIndexed(Index, Left, Bottom, Width, Height) -> 'ok' when Index :: integer(),Left :: integer(),Bottom :: integer(),Width :: integer(),Height :: integer().
scissorIndexed(Index,Left,Bottom,Width,Height) when is_integer(Index),is_integer(Left),is_integer(Bottom),is_integer(Width),is_integer(Height) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Left,Bottom,Width,Height,5850),
  ok.

%% @doc glScissorIndexe
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec scissorIndexedv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
scissorIndexedv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5851),
  ok.

%% @doc glDepthRangeArrayv
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec depthRangeArrayv(First, V) -> 'ok' when First :: integer(),V :: [{clamp(),clamp()}].
depthRangeArrayv(First,V) when is_integer(First),is_list(V) ->
  IF = get_interface(),
  Count = length(V),
  IF:queue_cmd(First,Count,V,5852),
  ok.

%% @doc glDepthRangeIndexe
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec depthRangeIndexed(Index, N, F) -> 'ok' when Index :: integer(),N :: clamp(),F :: clamp().
depthRangeIndexed(Index,N,F) when is_integer(Index),is_float(N),is_float(F) ->
  IF = get_interface(),
  IF:queue_cmd(Index,N,F,5853),
  ok.

%% @doc glGet
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getFloati_v(Target, Index) -> [float()] when Target :: enum(),Index :: integer().
getFloati_v(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5854),
  rec(5854).

%% @doc glGet
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getDoublei_v(Target, Index) -> [float()] when Target :: enum(),Index :: integer().
getDoublei_v(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5855),
  rec(5855).

%% @doc glDebugMessageControlARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec debugMessageControlARB(Source, Type, Severity, Ids, Enabled) -> 'ok' when Source :: enum(),Type :: enum(),Severity :: enum(),Ids :: [integer()],Enabled :: 0|1.
debugMessageControlARB(Source,Type,Severity,Ids,Enabled) when is_integer(Source),is_integer(Type),is_integer(Severity),is_list(Ids),(0 =:= Enabled) orelse (1 =:= Enabled) ->
  IF = get_interface(),
  Count = length(Ids),
  IF:queue_cmd(Source,Type,Severity,Count,Ids,Enabled,5856),
  ok.

%% @doc glDebugMessageInsertARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec debugMessageInsertARB(Source, Type, Id, Severity, Buf) -> 'ok' when Source :: enum(),Type :: enum(),Id :: integer(),Severity :: enum(),Buf :: string().
debugMessageInsertARB(Source,Type,Id,Severity,Buf) when is_integer(Source),is_integer(Type),is_integer(Id),is_integer(Severity),is_list(Buf) ->
  IF = get_interface(),
  BufBin = unicode:characters_to_binary([Buf|[0]]),
  IF:queue_cmd(Source,Type,Id,Severity,BufBin,5857),
  ok.

%% @doc glGetDebugMessageLogARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getDebugMessageLogARB(Count, Bufsize) -> {integer(),Sources :: [enum()],Types :: [enum()],Ids :: [integer()],Severities :: [enum()],MessageLog :: string()} when Count :: integer(),Bufsize :: integer().
getDebugMessageLogARB(Count,Bufsize) when is_integer(Count),is_integer(Bufsize) ->
  IF = get_interface(),
  IF:queue_cmd(Count,Bufsize,5858),
  rec(5858).

%% @doc glGetGraphicsResetStatusARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getGraphicsResetStatusARB() -> enum().
getGraphicsResetStatusARB()  ->
  IF = get_interface(),
  IF:queue_cmd(5859),
  rec(5859).

%% @doc glDrawArraysInstancedBaseInstance
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec drawArraysInstancedBaseInstance(Mode, First, Count, Primcount, Baseinstance) -> 'ok' when Mode :: enum(),First :: integer(),Count :: integer(),Primcount :: integer(),Baseinstance :: integer().
drawArraysInstancedBaseInstance(Mode,First,Count,Primcount,Baseinstance) when is_integer(Mode),is_integer(First),is_integer(Count),is_integer(Primcount),is_integer(Baseinstance) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,First,Count,Primcount,Baseinstance,5860),
  ok.

%% @doc glDrawElementsInstancedBaseInstance
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec drawElementsInstancedBaseInstance(Mode, Count, Type, Indices, Primcount, Baseinstance) -> 'ok' when Mode :: enum(),Count :: integer(),Type :: enum(),Indices :: offset()|mem(),Primcount :: integer(),Baseinstance :: integer().
drawElementsInstancedBaseInstance(Mode,Count,Type,Indices,Primcount,Baseinstance) when is_integer(Mode),is_integer(Count),is_integer(Type),is_integer(Indices) orelse is_tuple(Indices) orelse is_binary(Indices),is_integer(Primcount),is_integer(Baseinstance) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Count,Type,Indices,Primcount,Baseinstance,5861),
  ok.

%% @doc glDrawElementsInstancedBaseVertexBaseInstance
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec drawElementsInstancedBaseVertexBaseInstance(Mode, Count, Type, Indices, Primcount, Basevertex, Baseinstance) -> 'ok' when Mode :: enum(),Count :: integer(),Type :: enum(),Indices :: offset()|mem(),Primcount :: integer(),Basevertex :: integer(),Baseinstance :: integer().
drawElementsInstancedBaseVertexBaseInstance(Mode,Count,Type,Indices,Primcount,Basevertex,Baseinstance) when is_integer(Mode),is_integer(Count),is_integer(Type),is_integer(Indices) orelse is_tuple(Indices) orelse is_binary(Indices),is_integer(Primcount),is_integer(Basevertex),is_integer(Baseinstance) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Count,Type,Indices,Primcount,Basevertex,Baseinstance,5863),
  ok.

%% @doc glDrawTransformFeedbackInstance
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec drawTransformFeedbackInstanced(Mode, Id, Primcount) -> 'ok' when Mode :: enum(),Id :: integer(),Primcount :: integer().
drawTransformFeedbackInstanced(Mode,Id,Primcount) when is_integer(Mode),is_integer(Id),is_integer(Primcount) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Id,Primcount,5865),
  ok.

%% @doc glDrawTransformFeedbackStreamInstance
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec drawTransformFeedbackStreamInstanced(Mode, Id, Stream, Primcount) -> 'ok' when Mode :: enum(),Id :: integer(),Stream :: integer(),Primcount :: integer().
drawTransformFeedbackStreamInstanced(Mode,Id,Stream,Primcount) when is_integer(Mode),is_integer(Id),is_integer(Stream),is_integer(Primcount) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Id,Stream,Primcount,5866),
  ok.

%% @doc glGetInternalformat
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getInternalformativ(Target, Internalformat, Pname, BufSize) -> [integer()] when Target :: enum(),Internalformat :: enum(),Pname :: enum(),BufSize :: integer().
getInternalformativ(Target,Internalformat,Pname,BufSize) when is_integer(Target),is_integer(Internalformat),is_integer(Pname),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Pname,BufSize,5867),
  rec(5867).

%% @doc glBindImageTexture
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec bindImageTexture(Unit, Texture, Level, Layered, Layer, Access, Format) -> 'ok' when Unit :: integer(),Texture :: integer(),Level :: integer(),Layered :: 0|1,Layer :: integer(),Access :: enum(),Format :: enum().
bindImageTexture(Unit,Texture,Level,Layered,Layer,Access,Format) when is_integer(Unit),is_integer(Texture),is_integer(Level),(0 =:= Layered) orelse (1 =:= Layered),is_integer(Layer),is_integer(Access),is_integer(Format) ->
  IF = get_interface(),
  IF:queue_cmd(Unit,Texture,Level,Layered,Layer,Access,Format,5868),
  ok.

%% @doc glMemoryBarrier
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec memoryBarrier(Barriers) -> 'ok' when Barriers :: integer().
memoryBarrier(Barriers) when is_integer(Barriers) ->
  IF = get_interface(),
  IF:queue_cmd(Barriers,5869),
  ok.

%% @doc glTexStorage1D
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texStorage1D(Target, Levels, Internalformat, Width) -> 'ok' when Target :: enum(),Levels :: integer(),Internalformat :: enum(),Width :: integer().
texStorage1D(Target,Levels,Internalformat,Width) when is_integer(Target),is_integer(Levels),is_integer(Internalformat),is_integer(Width) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Levels,Internalformat,Width,5870),
  ok.

%% @doc glTexStorage2D
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texStorage2D(Target, Levels, Internalformat, Width, Height) -> 'ok' when Target :: enum(),Levels :: integer(),Internalformat :: enum(),Width :: integer(),Height :: integer().
texStorage2D(Target,Levels,Internalformat,Width,Height) when is_integer(Target),is_integer(Levels),is_integer(Internalformat),is_integer(Width),is_integer(Height) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Levels,Internalformat,Width,Height,5871),
  ok.

%% @doc glTexStorage3D
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texStorage3D(Target, Levels, Internalformat, Width, Height, Depth) -> 'ok' when Target :: enum(),Levels :: integer(),Internalformat :: enum(),Width :: integer(),Height :: integer(),Depth :: integer().
texStorage3D(Target,Levels,Internalformat,Width,Height,Depth) when is_integer(Target),is_integer(Levels),is_integer(Internalformat),is_integer(Width),is_integer(Height),is_integer(Depth) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Levels,Internalformat,Width,Height,Depth,5872),
  ok.

%% @doc glDepthBoundsEXT
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec depthBoundsEXT(Zmin, Zmax) -> 'ok' when Zmin :: clamp(),Zmax :: clamp().
depthBoundsEXT(Zmin,Zmax) when is_float(Zmin),is_float(Zmax) ->
  IF = get_interface(),
  IF:queue_cmd(Zmin,Zmax,5873),
  ok.

%% @doc glStencilClearTagEXT
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec stencilClearTagEXT(StencilTagBits, StencilClearTag) -> 'ok' when StencilTagBits :: integer(),StencilClearTag :: integer().
stencilClearTagEXT(StencilTagBits,StencilClearTag) when is_integer(StencilTagBits),is_integer(StencilClearTag) ->
  IF = get_interface(),
  IF:queue_cmd(StencilTagBits,StencilClearTag,5874),
  ok.

