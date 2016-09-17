using Calc.Lib;
using System;
using System.ComponentModel;

namespace Calc.Example.View
{
    internal class CustomPropertyDesc : PropertyDescriptor
    {
        private static readonly Random rnd = new Random();
        private int val;
        private static Func<IReferenceAccessor, string> _func = delegate { return ""; };
        public static Func<IReferenceAccessor, string> func { get
            {
                return _func;
            }
            set
            {
                _func = value;
                onFuncChanged();
            }
        }

        private static event Action onFuncChanged = delegate { };
        private readonly IReferenceAccessor accessor;
        private readonly Type type;

        public CustomPropertyDesc(TypeChecker.RefDef value, IReferenceAccessor accessor, Action<string> propertyChanged) : base(value.Name, new Attribute[0])
        {
            this.accessor = accessor;
            type = value.Type.GetBCLType;
            val = rnd.Next(0,100);
            onFuncChanged += () => propertyChanged(value.Name);
        }

        public override Type ComponentType
        {
            get
            {
                throw new NotImplementedException();
            }
        }

        public override bool IsReadOnly => Name != "calc";

        public override Type PropertyType => type;

        public override bool CanResetValue(object component) => false;

        public override object GetValue(object component) {
            if(Name=="calc"){
                return func(accessor);
            }
            else
                return val;
         }

        public override void ResetValue(object component)
        {
            throw new NotImplementedException();
        }

        public override void SetValue(object component, object value) => val = (int)value;

        public override bool ShouldSerializeValue(object component) => false;
    }
}