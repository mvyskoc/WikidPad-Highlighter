# Syntax highlighting of the source code
# currently are supported only: 
#   MSSQL, Python syntax, text (without formatting)
#
# Source version 0 - 10. July 2013
#
# Install:
#   Put the source into "WikidPad/user_extensions/highlighter.py" and restart WikidPad 
#
# Usage:
# In the WikiDpad write:
#   [:highlighting: "my_file.sql"; mssql; cp1250; showlines]
# where:
#   my_file   - it can be absulote or relative path. For the case of relative path
#               the base directory is directory of opened wiki.
#   mssql     - Type of source code and currently it can also be:
#                 python, text
#   cp1250    - Encoding of the source file. If it is not given utf8 is assumed.
#               For Windows binary of the WikiDpad can be necessary copy
#               appropriate source python files into the librabry.zip\encodings
#   showlines - If this word is stated, line numbers will be shown.


# Author: Martin Vyskocil   
#         m.vyskoc@seznam.cz

        


import os
import wx
import codecs
import re

WIKIDPAD_PLUGIN = (("InsertionByKey", 1),)


BOLD = 1 << 0           #Bold
ITALICA = 1 << 1        #Italica
UNDERLINE = 1 << 2      #Underline

class Style(object):
    def __init__(self, flags=0, **kwargs):
        """
            flags = B | I | U
            kwargs = color, bgcolor
        """
        self.options = kwargs
        self.flags = flags

    def isBold(self):
        return (self.flags & BOLD) == BOLD

    def isItalica(self):
        return (self.flags & ITALICA) == ITALICA

    def isUnderline(self):
        return (self.flags & UNDERLINE) == UNDERLINE



def keywords(word_list):
    return ['(?:(?<=\A)%s(?=\W))|(?:(?<=\W)%s(?=\W))' % (w,w) for w in word_list]


NUMBER=['(?<=[\s\=\+\-\(\)\[\],])(\d+(\.\d+)?)([eE][\+-]?\d+)?(?=[\s\=\+\-\(\)\[\],])']

PYTHON_KEYWORDS  = keywords(
                    ['and', 'del',  'from', 'not', 'while',
                    'as',  'elif', 'global', 'or', 'with',
                    'assert', 'else', 'if', 'pass', 'yield'
                    'break',  'except', 'import', 'print',
                    'class', 'exec', 'in', 'raise',
                    'continue', 'finally', 'is', 'return',
                    'def', 'for', 'lambda', 'try'])

PYTHON_STRING = [r'\"[^\n\"]*?\"', r"\'[^\n\']*?\'"]
PYTHON_RAW_STRING = [r'\"\"\".*?\"\"\"']
PYTHON_COMMENT = ['#[^\n]*']
PYTHON_FCE = ['(?<=def)(?:\s+)[a-zA-Z0-9_\Z]+']

SQL_COMMENT = ['#[^\n]*', '/\*.*?\*/', '--[^\n]*']
MSSQL_STRING = [r"\'[^\n\']*?\'"]
MSSQL_KEYWORDS = keywords(
    ['ABSOLUTE', 'ACTION', 'ADA', 'ADD', 'ADMIN', 'AFTER',
     'AGGREGATE', 'ALIAS', 'ALL', 'ALLOCATE', 'ALTER', 'AND',
     'ANY', 'ARE', 'ARRAY', 'AS', 'ASC', 'ASENSITIVE', 'ASSERTION',
     'ASYMMETRIC', 'AT', 'ATOMIC', 'AUTHORIZATION', 'BACKUP',
     'BEFORE', 'BEGIN', 'BETWEEN', 'BIT_LENGTH', 'BLOB',
     'BOOLEAN', 'BOTH', 'BREADTH', 'BREAK', 'BROWSE', 'BULK',
     'BY', 'CALL', 'CALLED', 'CARDINALITY', 'CASCADE', 'CASCADED',
     'CASE', 'CATALOG', 'CHAR_LENGTH', 'CHARACTER', 'CHARACTER_LENGTH',
     'CHECK', 'CHECKPOINT', 'CLASS', 'CLOB', 'CLOSE', 'CLUSTERED',
     'COALESCE', 'COLLATE', 'COLLATION', 'COLLECT', 'COLUMN',
     'COMMIT', 'COMPLETION', 'COMPUTE', 'CONDITION', 'CONNECT',
     'CONNECTION', 'CONSTRAINT', 'CONSTRAINTS', 'CONSTRUCTOR',
     'CONTAINS', 'CONTAINSTABLE', 'CONTINUE', 'CONVERT',
     'CORR', 'CORRESPONDING', 'COVAR_POP', 'COVAR_SAMP',
     'CREATE', 'CROSS', 'CUBE', 'CUME_DIST', 'CURRENT',
     'CURRENT_CATALOG', 'CURRENT_DATE', 'CURRENT_DEFAULT_TRANSFORM_GROUP',
     'CURRENT_PATH', 'CURRENT_ROLE', 'CURRENT_SCHEMA', 'CURRENT_TIME',
     'CURRENT_TRANSFORM_GROUP_FOR_TYPE', 'CYCLE', 'DATA',
     'DATABASE', 'DBCC', 'DEALLOCATE', 'DEC', 'DECLARE',
     'DEFAULT', 'DEFERRABLE', 'DEFERRED', 'DELETE', 'DENY',
     'DEPTH', 'DEREF', 'DESC', 'DESCRIBE', 'DESCRIPTOR',
     'DESTROY', 'DESTRUCTOR', 'DETERMINISTIC', 'DIAGNOSTICS',
     'DICTIONARY', 'DISCONNECT', 'DISK', 'DISTINCT', 'DISTRIBUTED',
     'DOMAIN', 'DOUBLE', 'DROP', 'DUMP', 'DYNAMIC', 'EACH',
     'ELEMENT', 'ELSE', 'END', 'END-EXEC', 'EQUALS', 'ERRLVL',
     'ESCAPE', 'EVERY', 'EXCEPT', 'EXCEPTION', 'EXEC', 'EXECUTE',
     'EXISTS', 'EXIT', 'EXTERNAL', 'EXTRACT', 'FALSE', 'FETCH',
     'FILE', 'FILLFACTOR', 'FILTER', 'FIRST', 'FOR', 'FOREIGN',
     'FORTRAN', 'FOUND', 'FREE', 'FREETEXT', 'FREETEXTTABLE',
     'FROM', 'FULL', 'FULLTEXTTABLE', 'FUNCTION', 'FUSION',
     'GENERAL', 'GET', 'GLOBAL', 'GO', 'GOTO', 'GRANT',
     'GROUP', 'HAVING', 'HOLD', 'HOLDLOCK', 'HOST', 'HOUR',
     'IDENTITY', 'IDENTITY_INSERT', 'IDENTITYCOL', 'IF',
     'IGNORE', 'IMMEDIATE', 'IN', 'INCLUDE', 'INDEX', 'INDICATOR',
     'INITIALIZE', 'INITIALLY', 'INNER', 'INOUT', 'INPUT',
     'INSENSITIVE', 'INSERT', 'INTEGER', 'INTERSECT', 'INTERSECTION',
     'INTERVAL', 'INTO', 'IS', 'ISOLATION', 'ITERATE', 'JOIN',
     'KEY', 'KILL', 'LANGUAGE', 'LARGE', 'LAST', 'LATERAL',
     'LEADING', 'LESS', 'LEVEL', 'LIKE', 'LIKE_REGEX', 'LIMIT',
     'LINENO', 'LN', 'LOAD', 'LOCAL', 'LOCALTIME', 'LOCALTIMESTAMP',
     'LOCATOR', 'MAP', 'MATCH', 'MEMBER', 'MERGE', 'METHOD',
     'MINUTE', 'MOD', 'MODIFIES', 'MODIFY', 'MODULE', 'MULTISET',
     'NAMES', 'NATIONAL', 'NATURAL', 'NCLOB', 'NEW', 'NEXT',
     'NO', 'NOCHECK', 'NONCLUSTERED', 'NONE', 'NORMALIZE',
     'NOT', 'NULL', 'NULLIF', 'OBJECT', 'OCCURRENCES_REGEX',
     'OCTET_LENGTH', 'OF', 'OFF', 'OFFSETS', 'OLD', 'ON',
     'ONLY', 'OPEN', 'OPERATION', 'OPTION', 'OR', 'ORDER',
     'ORDINALITY', 'OUT', 'OUTER', 'OUTPUT', 'OVER', 'OVERLAPS',
     'OVERLAY', 'PAD', 'PARAMETER', 'PARAMETERS', 'PARTIAL',
     'PARTITION', 'PASCAL', 'PATH', 'PERCENT', 'PERCENT_RANK',
     'PERCENTILE_CONT', 'PERCENTILE_DISC', 'PIVOT', 'PLAN',
     'POSITION', 'POSITION_REGEX', 'POSTFIX', 'PRECISION',
     'PREFIX', 'PREORDER', 'PREPARE', 'PRESERVE', 'PRIMARY',
     'PRINT', 'PRIOR', 'PRIVILEGES', 'PROC', 'PROCEDURE',
     'PUBLIC', 'RAISERROR', 'RANGE', 'READ', 'READS', 'READTEXT',
     'RECONFIGURE', 'RECURSIVE', 'REF', 'REFERENCES', 'REFERENCING',
     'REGR_AVGX', 'REGR_AVGY', 'REGR_COUNT', 'REGR_INTERCEPT',
     'REGR_R2', 'REGR_SLOPE', 'REGR_SXX', 'REGR_SXY', 'REGR_SYY',
     'RELATIVE', 'RELEASE', 'REPLICATION', 'RESTORE', 'RESTRICT',
     'RESULT', 'RETURN', 'RETURNS', 'REVERT', 'REVOKE',
     'ROLE', 'ROLLBACK', 'ROLLUP', 'ROUTINE', 'ROW', 'ROWCOUNT',
     'ROWGUIDCOL', 'ROWS', 'RULE', 'SAVE', 'SAVEPOINT',
     'SCHEMA', 'SCOPE', 'SCROLL', 'SEARCH', 'SECOND', 'SECTION',
     'SECURITYAUDIT', 'SELECT', 'SEMANTICKEYPHRASETABLE',
     'SEMANTICSIMILARITYDETAILSTABLE', 'SEMANTICSIMILARITYTABLE',
     'SENSITIVE', 'SEQUENCE', 'SESSION', 'SET', 'SETS',
     'SETUSER', 'SHUTDOWN', 'SIMILAR', 'SIZE', 'SOME', 'SPECIFIC',
     'SPECIFICTYPE', 'SQL', 'SQLCA', 'SQLCODE', 'SQLERROR',
     'SQLEXCEPTION', 'SQLSTATE', 'SQLWARNING', 'START',
     'STATE', 'STATEMENT', 'STATIC', 'STATISTICS', 'STDDEV_POP',
     'STDDEV_SAMP', 'STRUCTURE', 'SUBMULTISET', 'SUBSTRING_REGEX',
     'SYMMETRIC', 'SYSTEM', 'TABLESAMPLE', 'TEMPORARY',
     'TERMINATE', 'TEXTSIZE', 'THAN', 'THEN', 'TIMEZONE_HOUR',
     'TIMEZONE_MINUTE', 'TO', 'TOP', 'TRAILING', 'TRAN',
     'TRANSACTION', 'TRANSLATE', 'TRANSLATE_REGEX', 'TRANSLATION',
     'TREAT', 'TRIGGER', 'TRIM', 'TRUE', 'TRUNCATE', 'TSEQUAL',
     'UESCAPE', 'UNDER', 'UNION', 'UNIQUE', 'UNKNOWN', 'UNNEST',
     'UNPIVOT', 'UPDATE', 'UPDATETEXT', 'USAGE', 'USE',
     'USER', 'USING', 'VALUE', 'VALUES', 'VAR_POP', 'VAR_SAMP',
     'VARIABLE', 'VARYING', 'VIEW', 'WAITFOR', 'WHEN', 'WHENEVER',
     'WHERE', 'WHILE', 'WIDTH_BUCKET', 'WINDOW', 'WITH',
     'WITHIN', 'WITHIN GROUP', 'WITHOUT', 'WORK', 'WRITE',
     'WRITETEXT', 'XMLAGG', 'XMLATTRIBUTES', 'XMLBINARY',
     'XMLCAST', 'XMLCOMMENT', 'XMLCONCAT', 'XMLDOCUMENT',
     'XMLELEMENT', 'XMLEXISTS', 'XMLFOREST', 'XMLITERATE',
     'XMLNAMESPACES', 'XMLPARSE', 'XMLPI', 'XMLQUERY', 'XMLSERIALIZE',
     'XMLTABLE', 'XMLTEXT', 'XMLVALIDATE', 'ZONE'])

MSSQL_TYPES = keywords(
    ['datetimeoffset', 'int', 'money', 'image', 'float',
     'numeric', 'datetime', 'hierarchyid', 'bigint', 'table',
     'xml', 'binary', 'smalldatetime', 'uniqueidentifier',
     'text', 'real', 'smallint', 'varchar', 'timestamp',
     'date', 'bit', 'sql_variant', 'decimal', 'datetime2',
     'cursor', 'tinyint', 'smallmoney', 'time', 'varbinary']
)

MSSQL_FCE = keywords(
    ['cot', 'varp', '@@lock_timeout', 'file_idex', 'eomonth',
     'month', 'charindex', 'count_big', 'is_srvrolemember',
     'sqrt', 'unicode', 'sysdatetime', '@@max_precision',
     'quotename', 'app_name', 'getutcdate', 'objectpropertyex',
     'host_id', 'sysdatetimeoffset', 'type_id', 'objectproperty',
     'error_procedure', 'binary_checksum', '@@timeticks',
     'formatmessage', 'degrees', 'choose', 'current_user',
     'min_active_rowversion', 'col_length', 'set datefirst',
     'sp_helplanguage', 'format', 'indexproperty', 'filegroupproperty',
     'is_rolemember', 'dateadd', '@@langid', '@@spid', 'rank',
     'getansinull', 'difference', 'databasepropertyex',
     'day', 'permissions', '@@total_errors', 'checksum_agg',
     'system_user', 'serverproperty', 'error_message', 'radians',
     '@@remserver', 'parsename', 'current_timestamp', 'newid',
     'host_name', 'certprivatekey', 'context_info', 'suser_id',
     '@@cursor_rows', 'round', '@@io_busy', 'grouping',
     'stdevp', 'upper', 'square', 'original_db_name', 'is_member',
     'scope_identity', 'sign', 'datetimeoffsetfromparts',
     'datetime2fromparts', 'year', 'typeproperty', 'asin',
     'getdate', '@@datefirst', 'datefromparts', 'sum', '@@total_write',
     'error_line', '@@pack_sent', 'timefromparts', '@@cpu_busy',
     'row_number', 'db_id', 'dense_rank', 'power', 'log10',
     'col_name', 'try_convert', 'suser_sname', 'len', 'applock_test',
     'rowcount_big', 'switchoffset', 'file_id', 'cursor_status',
     'stdev', 'connectionproperty', 'error_state', '@@textsize',
     'smalldatetimefromparts', 'set dateformat', 'rand',
     'reverse', 'index_col', 'checksum', 'newsequentialid',
     'schema_id', 'stuff', 'acos', 'sys.fn_builtin_permissions',
     'fulltextserviceproperty', 'current_request_id', 'isdate',
     'openrowset', 'object_schema_name', 'xact_state', 'session_user',
     'replace', 'isnull', 'sysutcdatetime', 'right', '@@pack_received',
     '@@dbts', 'database_principal_id', 'suser_sid', 'todatetimeoffset',
     'error_number', 'user_id', 'log', 'certencoded', 'suser_name',
     'type_name', 'object_name', 'original_login', 'sys.fn_get_audit_file',
     'replicate', 'datetimefromparts', 'file_name', 'var',
     'grouping_id', '@@fetch_status', 'user_name', '@@version',
     'try_parse', 'ceiling', '@@servername', 'cos', 'iif',
     'rtrim', 'openxml', 'ntile', 'count', 'char', 'pi',
     '@@language', 'textptr', 'pwdcompare', '@@max_connections',
     '@@idle', 'cast', 'object_definition', '@@rowcount',
     '@@error', 'str', 'exp', 'indexkey_property', 'opendatasource',
     'set language', 'next value for', 'abs', 'filegroup_id',
     '@@nestlevel', 'sys.fn_my_permissions', 'substring',
     'textvalid', '@@trancount', 'try_cast', '@@servicename',
     '@@connections', 'tan', 'avg', 'ascii', 'openquery',
     '@@packet_errors', 'has_perms_by_name', 'applock_mode',
     'min', '@@total_read', 'object_id', '@@options', 'atn2',
     'isnumeric', 'datediff', 'fileproperty', 'get_filestream_transaction_context',
     'patindex', 'datepart', '$partition', 'ltrim', 'atan',
     'max', 'fn_virtualfilestats', 'filegroup_name', 'fulltextcatalogproperty',
     'floor', 'columnproperty', '@@identity', 'stats_date',
     'concat', 'soundex', 'lower', 'parse', 'assemblyproperty',
     'space', 'datename', '@@procid', 'pwdencrypt', 'db_name',
     'schema_name', 'sin', 'error_severity', 'nchar', 'left']
)

SCHEMA = {
    'python' : {
        're_flags' : 0,    #0 nebo re.IGNORECASE
        'rules': [
            (PYTHON_KEYWORDS, Style(BOLD, color='00007f')),
            (PYTHON_FCE, Style(BOLD, color='007f7f')),
            (PYTHON_COMMENT, Style(color='007f00', bgcolor='e8ffe8')),
            (PYTHON_RAW_STRING, Style(color='7f0000')),
            (PYTHON_STRING, Style(color='7f007f')),
            (NUMBER, Style(BOLD, color='7f7f00'))
        ]
    },
    
    'mssql' : {
        're_flags' : re.IGNORECASE,    #0 nebo re.IGNORECASE
        'rules': [ 
            (SQL_COMMENT, Style(ITALICA, color='808080')),
            (MSSQL_STRING, Style(color='FF0000')),
            (MSSQL_KEYWORDS, Style(BOLD, color='0000FF')),
            (MSSQL_TYPES, Style(BOLD, color='8000FF')),
            (MSSQL_FCE, Style(BOLD, color='009B37')),
            (NUMBER, Style(BOLD, color='7f7f00'))
        ]
    },
    
    'text' : {
        're_flags' : 0,    #0 nebo re.IGNORECASE
        'rules': [ ]
    }
    
}

class RuleSubstitute(object):
    def __init__(self, rules, flags= re.DOTALL | re.MULTILINE):
        self.regex, self.replace = self.Compile(rules, flags)

    def _Lookup(self, match, fce):
        match_groups = match.groupdict()
        for id in match_groups:
            text = match_groups[id]
            if text:
                return fce(text, self.replace[id])
        return ''

    def sub(self, fce, text):
        f = lambda match: self._Lookup(match, fce)
        return self.regex.sub(f, text)

    def Compile(self, rules, flags):
        group_replacements = dict()

        regex_rules = []
        group_id = 0
        for group, replacement in rules:
            if isinstance(group, (str, unicode)):
                group = [group]

            group_re = '|'.join(['(?:'+i+')' for i in group])
            regex_rules.append('(?P<grp%s>%s)' % ( group_id, group_re) )
            group_replacements['grp'+str(group_id)] = replacement
            group_id += 1

        regex = '|'.join(regex_rules)
        compiled_re = re.compile(regex, flags)
        return compiled_re, group_replacements


class BaseFormatter(object):
    REPLACE_TABLE=[]

    def __init__(self, flags=re.DOTALL | re.MULTILINE):
        self.subs = RuleSubstitute(self.REPLACE_TABLE, flags)

    def _TextSubstitute(self, original, replace):
        if replace.find("%s") > 0:
            return replace % (original,)
        else:
            return replace

    def Text(self, text):
        return self.subs.sub(self._TextSubstitute, text)

    def Format(self, style, text):
        result = ''
        if text.strip() == '':
            return self.Text(text)
            
        linesep = self.Text('\n')
        
        if text.find(linesep) > 0:
            for line in text.split(linesep):
                result += self.Format(style, line) + linesep
        else:
            result = ''
        
        return result

class HTMLFormatter(BaseFormatter):
    REPLACE_TABLE = [
            ('\n', '<br>\n'),
            ('"', '&quot;'),
            ("'", '&apos;'),
            ("&", '&amp;'),
            ("\<", '&lt;'),
            ("\>", '&gt;'),
            ("\t", 8*'&nbsp;'),
            ("(?<= ) ", '&nbsp;')
    ]


    def Format(self, style, text):
        result = super(HTMLFormatter, self).Format(style, text)
        if result:
            return result
        
        result = self.Text(text)
        if style.isBold():
            result = "<b>" + result + "</b>"

        if style.isItalica():
            result = "<i>" + result + "</i>"

        if style.isUnderline():
            result = "<u>" + result + "</u>"

        font_tag = ''
        color = style.options.get('color')
        if color:
            font_tag += 'color="#%s"' % (color,)

        bgcolor = style.options.get('bgcolor')
        if bgcolor:
            font_tag += 'style="background-color:#%s;"' % (bgcolor,)

        if font_tag:
            result = "<font " + font_tag + ">" + result + "</font>"

        return result

class WIKIFormatter(BaseFormatter):
    REPLACE_TABLE = [
            ('\[', '\['),
            ('^\+', r'\+'),
            (r'\\', r'\\'),
            ('\*', r'\*'),
            ('_', r'\_'),
            ("\<", r'\<'),
            ("\>", r'\>'),
            ("\t", 8 * '&nbsp;'),
            (" ", '&nbsp;')
    ]


    def Format(self, style, text):
        result = super(WIKIFormatter, self).Format(style, text)
        if result:
            return result
        
        result = self.Text(text)
        
        if style.isBold():
            result = "*" + result + "*"

        if style.isItalica():
            result = "_" + result + "_"

        if style.isUnderline():
            result = "<u>" + result + "</u>"

        font_tag = ''
        color = style.options.get('color')
        if color:
            font_tag += 'color="#%s"' % (color,)

        bgcolor = style.options.get('bgcolor')
        if bgcolor:
            font_tag += 'style="background-color:#%s;"' % (bgcolor,)

        if font_tag:
            result = "<font " + font_tag + ">" + result + "</font>"

        return result


class SyntaxHighlighting(object):
    def __init__(self, language, formatter=HTMLFormatter()):
        try:
            schema = SCHEMA[language]
        except:
            raise RuntimeError("Syntax highlighting is not supported for language %s" % (language,))

        self.formatter = formatter
        rules = list(schema['rules'])
        rules.extend(formatter.REPLACE_TABLE)
        self.subs = RuleSubstitute(rules, schema['re_flags'] | re.DOTALL | re.MULTILINE )

    def TextStyle(self, old_text, style):
        if isinstance(style, Style):
            return self.formatter.Format(style, old_text)
        else:
            if style.find("%s") > 0:
                return style % (oldtext,)
            else:
                return style

    def ShowLines(self, text):
        line_sep = self.formatter.Text('\n')
        text_lines = text.split(line_sep)
        result = list()
        
        fmt = "%%.%dd:   " % (len(str(len(text_lines))),)
        line_no = 0
        for line in text_lines:
            result.append(self.formatter.Text(fmt % (line_no,)) + line)
            line_no += 1
            
        return line_sep.join(result)
        
    def Process(self, text, showlines=True):
        text = text.strip()
        new_text = self.subs.sub(self.TextStyle, text)
        
        if showlines:
            new_text = self.ShowLines(new_text)
 
        return "<code>%s</code>" % (new_text,)


def describeInsertionKeys(ver, app):
    """
    API function for "InsertionByKey" plugins
    Returns a sequence of tuples describing the supported
    insertion keys. Each tuple has the form (insKey, exportTypes, handlerFactory)
    where insKey is the insertion key handled, exportTypes is a sequence of
    strings describing the supported export types and handlerFactory is
    a factory function (normally a class) taking the wxApp object as
    parameter and returning a handler object fulfilling the protocol
    for "insertion by key" (see EqnHandler as example).
    
    This plugin uses the special export type "wikidpad_language" which is
    not a real type like HTML export, but allows to return a string
    which conforms to WikidPad wiki syntax and is postprocessed before
    exporting.
    Therefore this plugin is not bound to a specific export type.

    ver -- API version (can only be 1 currently)
    app -- wxApp object
    """
    return ((u"highlighting", ("wikidpad_language",), HighlightingHandler),)

class HighlightingHandler:
    """
    Class fulfilling the "insertion by key" protocol.
    """
    def __init__(self, app):
        self.app = app
        
    def taskStart(self, exporter, exportType):
        """
        This is called before any call to createContent() during an
        export task.
        An export task can be a single HTML page for
        preview or a single page or a set of pages for export.
        exporter -- Exporter object calling the handler
        exportType -- string describing the export type
        
        Calls to createContent() will only happen after a 
        call to taskStart() and before the call to taskEnd()
        """
        pass

        
    def taskEnd(self):
        """
        Called after export task ended and after the last call to
        createContent().
        """
        pass

              
    def createContent(self, exporter, exportType, insToken):
        """
        Handle an insertion and create the appropriate content.

        exporter -- Exporter object calling the handler
        exportType -- string describing the export type
        insToken -- insertion token to create content for

        An insertion token has the following member variables:
            key: insertion key (unistring)
            value: value of an insertion (unistring)
            appendices: sequence of strings with the appendices

        Meaning and type of return value is solely defined by the type
        of the calling exporter.
        
        For HtmlExporter a unistring is returned with the HTML code
        to insert instead of the insertion.        
        """
        baseDir = os.path.dirname(exporter.getMainControl().getWikiConfigPath())
        args = [None, "utf8"]
        wargs = list()
        for i, apx in enumerate(insToken.appendices):
            if i < 2:
              args[i] = apx
            elif i >= 2:
              wargs.append(apx.lower())
            else:
              return "Expected at least 1 arguments: language"

        file_name = insToken.value
        language = args[0]
        encoding = args[1].strip()
        showline = 'showlines' in wargs
        
        if not SCHEMA.has_key(language):
            return "Syntax highlighting is not supported for the language %s" % (language,)
        
        if os.path.isabs(file_name) == False:
            file_name = os.path.join(baseDir, file_name)
        
        if not os.path.exists(file_name):
            return "Can't find the file: %s" % ( file_name.replace('\\', '\\\\') ,)
            
        file = codecs.open(file_name, encoding=encoding)
        txt_data = file.read()
        file.close()
    
        highliting = SyntaxHighlighting(language, WIKIFormatter())
        return highliting.Process(txt_data, showline)


    def getExtraFeatures(self):
        """
        Returns a list of bytestrings describing additional features supported
        by the plugin. Currently not specified further.
        """
        return ()

