# -*- coding: utf-8 -*-
# Generated by the protocol buffer compiler.  DO NOT EDIT!
# source: scg.proto
"""Generated protocol buffer code."""
from google.protobuf import descriptor as _descriptor
from google.protobuf import descriptor_pool as _descriptor_pool
from google.protobuf import message as _message
from google.protobuf import reflection as _reflection
from google.protobuf import symbol_database as _symbol_database
# @@protoc_insertion_point(imports)

_sym_db = _symbol_database.Default()




DESCRIPTOR = _descriptor_pool.Default().AddSerializedFile(b'\n\tscg.proto\x12(com.virtuslab.semanticgraphs.proto.model\"i\n\x08Location\x12\x0b\n\x03uri\x18\x01 \x01(\t\x12\x11\n\tstartLine\x18\x02 \x01(\x05\x12\x16\n\x0estartCharacter\x18\x03 \x01(\x05\x12\x0f\n\x07\x65ndLine\x18\x04 \x01(\x05\x12\x14\n\x0c\x65ndCharacter\x18\x05 \x01(\x05\"f\n\x04\x45\x64ge\x12\n\n\x02to\x18\x01 \x01(\t\x12\x0c\n\x04type\x18\x02 \x01(\t\x12\x44\n\x08location\x18\x03 \x01(\x0b\x32\x32.com.virtuslab.semanticgraphs.proto.model.Location\"\xcb\x02\n\tGraphNode\x12\n\n\x02id\x18\x01 \x01(\t\x12\x0c\n\x04kind\x18\x02 \x01(\t\x12\x44\n\x08location\x18\x03 \x01(\x0b\x32\x32.com.virtuslab.semanticgraphs.proto.model.Location\x12W\n\nproperties\x18\x04 \x03(\x0b\x32\x43.com.virtuslab.semanticgraphs.proto.model.GraphNode.PropertiesEntry\x12\x13\n\x0b\x64isplayName\x18\x05 \x01(\t\x12=\n\x05\x65\x64ges\x18\x06 \x03(\x0b\x32..com.virtuslab.semanticgraphs.proto.model.Edge\x1a\x31\n\x0fPropertiesEntry\x12\x0b\n\x03key\x18\x01 \x01(\t\x12\r\n\x05value\x18\x02 \x01(\t:\x02\x38\x01\"d\n\x11SemanticGraphFile\x12\x0b\n\x03uri\x18\x01 \x01(\t\x12\x42\n\x05nodes\x18\x02 \x03(\x0b\x32\x33.com.virtuslab.semanticgraphs.proto.model.GraphNodeb\x06proto3')



_LOCATION = DESCRIPTOR.message_types_by_name['Location']
_EDGE = DESCRIPTOR.message_types_by_name['Edge']
_GRAPHNODE = DESCRIPTOR.message_types_by_name['GraphNode']
_GRAPHNODE_PROPERTIESENTRY = _GRAPHNODE.nested_types_by_name['PropertiesEntry']
_SEMANTICGRAPHFILE = DESCRIPTOR.message_types_by_name['SemanticGraphFile']
Location = _reflection.GeneratedProtocolMessageType('Location', (_message.Message,), {
  'DESCRIPTOR' : _LOCATION,
  '__module__' : 'scg_pb2'
  # @@protoc_insertion_point(class_scope:com.virtuslab.semanticgraphs.proto.model.Location)
  })
_sym_db.RegisterMessage(Location)

Edge = _reflection.GeneratedProtocolMessageType('Edge', (_message.Message,), {
  'DESCRIPTOR' : _EDGE,
  '__module__' : 'scg_pb2'
  # @@protoc_insertion_point(class_scope:com.virtuslab.semanticgraphs.proto.model.Edge)
  })
_sym_db.RegisterMessage(Edge)

GraphNode = _reflection.GeneratedProtocolMessageType('GraphNode', (_message.Message,), {

  'PropertiesEntry' : _reflection.GeneratedProtocolMessageType('PropertiesEntry', (_message.Message,), {
    'DESCRIPTOR' : _GRAPHNODE_PROPERTIESENTRY,
    '__module__' : 'scg_pb2'
    # @@protoc_insertion_point(class_scope:com.virtuslab.semanticgraphs.proto.model.GraphNode.PropertiesEntry)
    })
  ,
  'DESCRIPTOR' : _GRAPHNODE,
  '__module__' : 'scg_pb2'
  # @@protoc_insertion_point(class_scope:com.virtuslab.semanticgraphs.proto.model.GraphNode)
  })
_sym_db.RegisterMessage(GraphNode)
_sym_db.RegisterMessage(GraphNode.PropertiesEntry)

SemanticGraphFile = _reflection.GeneratedProtocolMessageType('SemanticGraphFile', (_message.Message,), {
  'DESCRIPTOR' : _SEMANTICGRAPHFILE,
  '__module__' : 'scg_pb2'
  # @@protoc_insertion_point(class_scope:com.virtuslab.semanticgraphs.proto.model.SemanticGraphFile)
  })
_sym_db.RegisterMessage(SemanticGraphFile)

if _descriptor._USE_C_DESCRIPTORS == False:

  DESCRIPTOR._options = None
  _GRAPHNODE_PROPERTIESENTRY._options = None
  _GRAPHNODE_PROPERTIESENTRY._serialized_options = b'8\001'
  _LOCATION._serialized_start=55
  _LOCATION._serialized_end=160
  _EDGE._serialized_start=162
  _EDGE._serialized_end=264
  _GRAPHNODE._serialized_start=267
  _GRAPHNODE._serialized_end=598
  _GRAPHNODE_PROPERTIESENTRY._serialized_start=549
  _GRAPHNODE_PROPERTIESENTRY._serialized_end=598
  _SEMANTICGRAPHFILE._serialized_start=600
  _SEMANTICGRAPHFILE._serialized_end=700
# @@protoc_insertion_point(module_scope)
