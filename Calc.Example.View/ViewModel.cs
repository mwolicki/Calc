using Calc.Lib;
using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Controls;
using System.Windows.Data;

namespace Calc.Example.View
{
    class ViewModel : System.ComponentModel.INotifyPropertyChanged
    {
  
        private string _expression;

        public string Expression
        {
            get
            {
                return _expression;
            }
            set
            {
                _expression = value;
                OpPropertyChanged();
                Recompile(value);
            }
        }

        private Task<Core.Result<Delegate, string>> RecompileAsync<T>(string code)
        {
            return Task.Run(() => {
                var funcs = Compile.defaultFuncs;
                var refs = LineViewModel.refs;
                return Compile.compile<T>(funcs, refs, code);
            });
        }

        private volatile bool isRecompiling;
        private volatile string lastValue;
        private string _errors;

        private async void Recompile(string value)
        {
            if (lastValue == _expression)
                return;
            await Task.Delay(200);
            if (value != _expression || isRecompiling)
                return;
            try
            {
                var sw = System.Diagnostics.Stopwatch.StartNew();
                isRecompiling = true;
                Errors = "(...)";
                var res = await RecompileAsync<bool>(value);
                if (res.IsOK)
                {
                    var p = (Core.Result<Delegate, string>.OK)res;
                    CustomPropertyDesc.func = ToFunc((dynamic)p.Item);
                    sw.Stop();
                    Errors = sw.Elapsed.ToString();
                    OpPropertyChanged("Lines");
                }
                else
                {

                    var p = (Core.Result<Delegate, string>.Error)res;
                    Errors = p.Item;
                }
            }
            catch(Exception e)
            {
                Errors = e.ToString();
            }
            finally
            {
                lastValue = value;
                isRecompiling = false;
            }
        }

        private Func<IReferenceAccessor, string> ToFunc(Func<IReferenceAccessor, string> item)=> item;
        private Func<IReferenceAccessor, string> ToFunc<T>(Func<IReferenceAccessor, T> item)
            => x=> item(x).ToString();

        public string Errors
        {
            get
            {
                return _errors;
            }
            set
            {
                _errors = value;
                OpPropertyChanged();
            }
        }

        public string Examples
        {
            get
            {
                return _expression;
            }
            set
            {
                _expression = value;
                OpPropertyChanged(value);
            }
        }

        public LineViewModel[] Lines { get; set; }

        public ViewModel()
        {
            Lines = Enumerable.Range(0, 1000).Select(_ => new LineViewModel()).ToArray();
        }


        private void OpPropertyChanged([CallerMemberName] string name = null)
        {
            PropertyChanged(this, new PropertyChangedEventArgs(name));
        }

        public event PropertyChangedEventHandler PropertyChanged = delegate { };
    }


    class LineViewModel : CustomTypeDescriptor, INotifyPropertyChanged
    {
        public static Random rnd = new Random();
        public static IReadOnlyDictionary<string, TypeChecker.RefDef> refs =
            new Dictionary<string, TypeChecker.RefDef>
            {
                { "number", new TypeChecker.RefDef("number", TypeChecker.Type.Integer) },
                { "number 2", new TypeChecker.RefDef("number 2", TypeChecker.Type.Integer) },
                { "decimal", new TypeChecker.RefDef("decimal", TypeChecker.Type.Decimal) },
                { "decimal 2", new TypeChecker.RefDef("decimal 2", TypeChecker.Type.Decimal) },
                //{ "text", new TypeChecker.RefDef("text", TypeChecker.Type.String) },
                //{ "text 2", new TypeChecker.RefDef("text 2", TypeChecker.Type.String) },
                { "calc", new TypeChecker.RefDef("calc", TypeChecker.Type.String) },
            };


        private PropertyDescriptorCollection props;

        public event PropertyChangedEventHandler PropertyChanged = delegate { };

        public override PropertyDescriptorCollection GetProperties()
        {
            return props ?? (props = new PropertyDescriptorCollection(GetPropertiesArray()));
        }
        

        public override PropertyDescriptorCollection GetProperties(Attribute[] attributes)
        {
            return GetProperties();
        }
        private PropertyDescriptor[] GetPropertiesArray()
        {
            var accessor = new ReferenceAccessor();
            var props = refs.Select(x => new CustomPropertyDesc(x.Value, accessor, OnPropertyChanged)).Cast<PropertyDescriptor>().ToArray();
            foreach(var prop in props)
            {
                accessor.Register(prop);
            }
            return props;
        }

        private void OnPropertyChanged([CallerMemberName] string name = null)
        {
            PropertyChanged(this, new PropertyChangedEventArgs(name));
        }

    }

    class ReferenceAccessor : IReferenceAccessor
    {
        public void Register(PropertyDescriptor desc)
        {
            descs[desc.Name] = desc;
        }

        private Dictionary<string, PropertyDescriptor> descs = new Dictionary<string, PropertyDescriptor>();

        private T GetValue<T>(string name) => (T)descs[name].GetValue(null);

        public bool GetBoolean(string value) => GetValue<bool>(value);

        public decimal GetDecimal(string value) => GetValue<decimal>(value);

        public int GetInt(string value) => GetValue<int>(value);

        public string GetString(string value) => GetValue<string>(value);

        public Date GetDate(string value) => new Date(2,2,2);

        public DateTime GetDateTime(string value) => DateTime.Now;

        public a Get<a>(string value)
        {
            throw new NotImplementedException();
        }
    }
}
